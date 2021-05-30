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
import qualified Data.HashMap.Strict as HM
import Dhall hiding (maybe)
import qualified Dhall.TH
import qualified Gerrit.Event as Gerrit
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
    configFile :: w ::: FilePath <?> "The gerritbot.dhall path"
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
    meRoomId :: Matrix.RoomID
  }
  deriving (Show, Eq)

-- | Prepare a Matrix Event
toMatrixEvent :: Gerrit.Change -> Gerrit.User -> Gerrit.Event -> Matrix.RoomID -> MatrixEvent
toMatrixEvent Gerrit.Change {..} user event meRoomId = MatrixEvent {..}
  where
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
getEventRoom :: Gerrit.EventType -> Gerrit.Change -> Channel -> Maybe Matrix.RoomID
getEventRoom eventType Gerrit.Change {..} Channel {..}
  | projectMatch && branchMatch && eventMatch = Just (Matrix.RoomID roomId)
  | otherwise = Nothing
  where
    match eventValue confValue = glob (toString confValue) (toString eventValue)
    projectMatch = any (match changeProject) projects
    branchMatch = any (match changeBranch) branches
    eventMatch = any (eventEquals eventType) events

-- | The gerritbot callback
onEvent :: [Channel] -> TBMQueue MatrixEvent -> Gerrit.Event -> IO ()
onEvent channels tqueue event =
  case (Gerrit.getChange event, Gerrit.getUser event) of
    (Just change, Just user) -> do
      putTextLn $ "Processing " <> show event
      case mapMaybe (getEventRoom (Gerrit.getEventType event) change) channels of
        [] -> putTextLn "No channel matched"
        xs -> do
          putTextLn $ "Queuing notification to " <> show xs
          mapM_ (queueMessage $ toMatrixEvent change user event) xs
    _ -> pure mempty
  where
    queueMessage mkMatrixEvent roomId = do
      atomically $ writeTBMQueue tqueue (mkMatrixEvent roomId)

-- | Group events by room id and author
-- See: https://hackage.haskell.org/package/relude-1.0.0.1/docs/Relude-Extra-Group.html#v:groupBy
groupEvents :: [MatrixEvent] -> [(Matrix.RoomID, EventAction, [EventObject])]
groupEvents events = fmap toGroup $ concat $ groupUserEvent <$> groupRoomEvents events
  where
    toGroup :: NonEmpty MatrixEvent -> (Matrix.RoomID, EventAction, [EventObject])
    toGroup (x :| xs) = (meRoomId x, meAction x, fmap meObject (x : xs))
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
    send :: (Matrix.RoomID, EventAction, [EventObject]) -> IO ()
    send (roomID, author, messages) = do
      let messageDoc = formatMessages author messages
          text = renderText messageDoc
          html = renderHtml messageDoc
      res <- Matrix.sendMessage sess roomID text html
      print res

-- | gerritbot-matrix entrypoint
main :: IO ()
main = do
  args <- unwrapRecord "Gerritbot Matrix"
  token <- fromMaybe (error "Missing MATRIX_TOKEN environment") <$> lookupEnv "MATRIX_TOKEN"
  sess <- Matrix.createSession (matrixUrl args) $! toText token
  channels <- Dhall.input auto (toText $ configFile args)
  tqueue <- newTBMQueueIO 2048
  Async.concurrently_
    (runGerrit (Gerritbot.GerritServer (gerritHost args) (gerritUser args)) tqueue channels)
    (forever $ runMatrix sess tqueue)
  putTextLn "Done."
  where
    runGerrit server tqueue channels =
      Gerritbot.runStreamClient server (onEvent channels tqueue)
    runMatrix sess tqueue = do
      logMsg "Waiting for events"
      events <- bufferQueueRead 5_000_000 tqueue
      sendEvents sess events
      threadDelay 100_000
