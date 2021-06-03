{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The gerritbot-matrix main entrypoint
module Gerritbot.Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueueIO, writeTBMQueue)
import Data.Bits ((.|.))
import Data.Digest.Pure.SHA (sha1, showDigest)
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Dhall hiding (maybe)
import qualified Dhall.TH
import qualified Gerrit.Event as Gerrit
import Gerritbot (GerritServer (..))
import qualified Gerritbot
import Gerritbot.Utils
import qualified Matrix
import Options.Generic
import Relude
import Relude.Extra.Group (groupBy)

-- | Command line interface
-- See: http://hackage.haskell.org/package/optparse-generic-1.4.4/docs/Options-Generic.html
data CLI w = CLI
  { gerritHost :: w ::: Text <?> "The gerrit host",
    gerritUser :: w ::: Text <?> "The gerrit username",
    matrixUrl :: w ::: Text <?> "The matrix url",
    configFile :: w ::: FilePath <?> "The gerritbot.dhall path",
    syncClient :: w ::: Bool <?> "Sync matrix status (join rooms)",
    createToken :: w ::: Bool <?> "Create identity token"
  }
  deriving stock (Generic)

instance ParseRecord (CLI Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

-- | Generate Haskell Type from Dhall Type
-- See: https://hackage.haskell.org/package/dhall-1.38.0/docs/Dhall-TH.html
Dhall.TH.makeHaskellTypes
  [ Dhall.TH.MultipleConstructors "EventType" "./src/EventType.dhall",
    Dhall.TH.SingleConstructor "Channel" "Channel" "(./src/Config.dhall).Type"
  ]

deriving instance Eq EventType

-- | Compare gerritbot event type config with original gerrit type
eventEquals :: Gerrit.EventType -> EventType -> Bool
eventEquals gerritEventType eventType = case (gerritEventType, eventType) of
  (Gerrit.PatchsetCreatedEvent, PatchsetCreated) -> True
  (Gerrit.ChangeMergedEvent, ChangeMerged) -> True
  _ -> False

newtype EventAction = EventAction Doc
  deriving (Show, Eq)
  deriving newtype (Hashable)

newtype EventObject = EventObject {unObject :: Doc}
  deriving (Show, Eq)
  deriving newtype (Hashable)

data MatrixEvent = MatrixEvent
  { meAction :: EventAction,
    meObject :: EventObject,
    meRoomId :: Matrix.RoomID,
    meTime :: Int64
  }
  deriving (Show, Eq)

-- | Prepare a Matrix Event
toMatrixEvent :: SystemTime -> Gerrit.Change -> Gerrit.User -> Gerrit.Event -> Matrix.RoomID -> MatrixEvent
toMatrixEvent (MkSystemTime now _) Gerrit.Change {..} user event meRoomId = MatrixEvent {..}
  where
    meTime = fromMaybe now (Gerrit.getCreatedOn event)
    meAction = EventAction . DocText $ author <> " " <> verb <> ":"
    author =
      fromMaybe
        "Anonymous User"
        (Gerrit.userName user <|> Gerrit.userUsername user <|> Gerrit.userEmail user)
    verb = case event of
      Gerrit.EventPatchsetCreated _ -> "proposed"
      Gerrit.EventChangeMerged _ -> "merged"
      Gerrit.EventChangeAbandoned _ -> "abandoned"
      _ -> "n/a"
    meObject = EventObject . DocBody $ changeInfo
    changeInfo =
      [ DocText $ "[" <> changeProject <> branch <> "] ",
        DocLink changeUrl changeSubject
      ]
    branch =
      if changeBranch `elem` ["master", "main"]
        then ""
        else " " <> changeBranch

formatMessages :: EventAction -> [EventObject] -> Doc
formatMessages (EventAction action) objects =
  DocBody [action, DocList $ fmap unObject objects]

-- | Find if a channel match a change, return the roomId
getEventRoom :: GerritServer -> Gerrit.EventType -> Gerrit.Change -> Channel -> Maybe Matrix.RoomID
getEventRoom GerritServer {..} eventType Gerrit.Change {..} Channel {..}
  | serverMatch && projectMatch && branchMatch && eventMatch = Just (Matrix.RoomID roomId)
  | otherwise = Nothing
  where
    match eventValue confValue = glob (toString confValue) (toString eventValue)
    serverMatch = any (match host) servers
    projectMatch = any (match changeProject) projects
    branchMatch = any (match changeBranch) branches
    eventMatch = any (eventEquals eventType) events

-- | The gerritbot callback
onEvent :: [Channel] -> TBMQueue MatrixEvent -> GerritServer -> Gerrit.Event -> IO ()
onEvent channels tqueue server event =
  case (Gerrit.getChange event, Gerrit.getUser event) of
    (Just change, Just user) -> do
      now <- getSystemTime
      putTextLn $ "Processing " <> show event
      case mapMaybe (getEventRoom server (Gerrit.getEventType event) change) channels of
        [] -> putTextLn "No channel matched"
        xs -> do
          putTextLn $ "Queuing notification to " <> show xs
          mapM_ (queueMessage $ toMatrixEvent now change user event) xs
    _ -> pure mempty
  where
    queueMessage mkMatrixEvent roomId = do
      atomically $ writeTBMQueue tqueue (mkMatrixEvent roomId)

-- | Group events by room id and author
-- See: https://hackage.haskell.org/package/relude-1.0.0.1/docs/Relude-Extra-Group.html#v:groupBy
groupEvents :: [MatrixEvent] -> [(Matrix.RoomID, EventAction, Int64, [EventObject])]
groupEvents events = fmap toGroup $ concat $ groupUserEvent <$> groupRoomEvents events
  where
    toGroup :: NonEmpty MatrixEvent -> (Matrix.RoomID, EventAction, Int64, [EventObject])
    toGroup (x :| xs) = (meRoomId x, meAction x, mkTransactionId (x : xs), fmap meObject (x : xs))
    mkTransactionId :: [MatrixEvent] -> Int64
    mkTransactionId = foldr (.|.) 0 . fmap meTime
    groupUserEvent :: [MatrixEvent] -> [NonEmpty MatrixEvent]
    groupUserEvent = HM.elems . groupBy meAction
    groupRoomEvents :: [MatrixEvent] -> [[MatrixEvent]]
    groupRoomEvents = fmap toList . HM.elems . groupBy meRoomId

-- | The matrix client
sendEvents :: Matrix.Session -> [MatrixEvent] -> IO ()
sendEvents sess events = do
  logMsg $ "Sending events " <> show events
  mapM_ send (groupEvents events)
  where
    send :: (Matrix.RoomID, EventAction, Int64, [EventObject]) -> IO ()
    send (roomID, author, ts, messages) = do
      let messageDoc = formatMessages author messages
          text = renderText messageDoc
          html = renderHtml messageDoc
          txnId = toText . showDigest . sha1 . toLazy . encodeUtf8 $ text <> show ts
      res <- Matrix.sendMessage sess roomID text html txnId
      print res

-- | Sync the matrix client
doSyncClient :: Matrix.Session -> [Channel] -> IO ()
doSyncClient sess = mapM_ joinRoom
  where
    joinRoom :: Channel -> IO ()
    joinRoom Channel {..} = do
      logMsg $ "Joining room " <> show roomId
      res <- Matrix.joinRoom sess (Matrix.RoomID roomId)
      print res

doCreateToken :: Matrix.Session -> IO Text
doCreateToken _sess = do
  error "TODO"

-- | gerritbot-matrix entrypoint
main :: IO ()
main = do
  args <- unwrapRecord "Gerritbot Matrix"
  token <- fromMaybe (error "Missing MATRIX_TOKEN environment") <$> lookupEnv "MATRIX_TOKEN"
  sess <- Matrix.createSession (matrixUrl args) $! toText token
  channels <- Dhall.input auto (toText $ configFile args)
  when (syncClient args) (doSyncClient sess channels)
  when (createToken args) (doCreateToken sess >>= putTextLn)
  idToken <- fromMaybe (error "Missing MATRIX_IDENTITY_TOKEN environment") <$> lookupEnv "MATRIX_IDENTITY_TOKEN"
  let _idSess = sess {Matrix.token = toText $! idToken}
  tqueue <- newTBMQueueIO 2048
  Async.concurrently_
    (runGerrit (Gerritbot.GerritServer (gerritHost args) (gerritUser args)) tqueue channels)
    (forever $ runMatrix sess tqueue)
  putTextLn "Done."
  where
    -- TODO: infer subscribe list from channels configuration
    eventList =
      [ Gerrit.ChangeAbandonedEvent,
        Gerrit.ChangeDeletedEvent,
        Gerrit.ChangeMergedEvent,
        Gerrit.ChangeRestoredEvent,
        Gerrit.PatchsetCreatedEvent
      ]
    runGerrit server tqueue channels =
      Gerritbot.runStreamClient server eventList (onEvent channels tqueue)
    runMatrix sess tqueue = do
      logMsg "Waiting for events"
      events <- bufferQueueRead 5_000_000 tqueue
      sendEvents sess events
      threadDelay 100_000
