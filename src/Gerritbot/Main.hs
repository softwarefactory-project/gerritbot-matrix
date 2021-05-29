{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The gerritbot-matrix main entrypoint
module Gerritbot.Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueueIO, writeTBMQueue)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import Dhall hiding (maybe)
import qualified Dhall.TH
import qualified Gerrit.Event as Gerrit
import qualified Gerritbot
import Gerritbot.Utils (bufferQueueRead, glob, logMsg)
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
Dhall.TH.makeHaskellTypes [Dhall.TH.SingleConstructor "Channel" "Channel" "(./src/Config.dhall).Type"]

-- | Create 'MatrixEvent' for a 'Gerrit.ChangeEvent'
toMatrixEvent :: Text -> Gerrit.ChangeEvent -> MatrixEvent
toMatrixEvent meRoomId Gerrit.ChangeEvent {..} = MatrixEvent {..}
  where
    (meAuthor, meMessage) = case changeEventType of
      Gerrit.PatchsetCreated -> (Just author, info)
      Gerrit.ChangeMerged -> (Nothing, "Merged " <> info)
    info = project <> " " <> branch <> ": " <> url
    url = Gerrit.changeUrl changeEventChange
    branch = Gerrit.changeBranch changeEventChange
    project = changeEventProject
    author = case changeEventUploader of
      Nothing -> "Unknown User"
      Just user ->
        fromMaybe
          "Anonymous User"
          (Gerrit.userName user <|> Gerrit.userUsername user <|> Gerrit.userEmail user)

formatMessages :: Maybe Text -> [Text] -> Text
formatMessages authorM messages =
  case authorM of
    Just author ->
      author <> " proposed:" <> case messages of
        [x] -> " " <> x
        xs -> mconcat (mappend "\n- " <$> xs)
    Nothing -> case messages of
      [x] -> x
      xs -> Text.intercalate "\n" (mappend "- " <$> xs)

data MatrixEvent = MatrixEvent
  { meAuthor :: Maybe Text,
    meMessage :: Text,
    meRoomId :: Text
  }
  deriving (Show, Eq)

-- | Find if a channel match an event, return the roomId
getEventRoom :: Gerrit.ChangeEvent -> Channel -> Maybe Text
getEventRoom Gerrit.ChangeEvent {..} Channel {..}
  | projectMatch && branchMatch = Just roomId
  | otherwise = Nothing
  where
    match eventValue confValue = glob (toString confValue) (toString eventValue)
    projectMatch = any (match changeEventProject) projects
    branchMatch = any (match $ Gerrit.changeBranch changeEventChange) branches

-- | The gerritbot callback
onEvent :: [Channel] -> TBMQueue MatrixEvent -> Gerrit.Event -> IO ()
onEvent channels tqueue event = case event of
  Gerrit.EventChange changeEvent -> do
    putTextLn $ "Processing " <> show event
    case mapMaybe (getEventRoom changeEvent) channels of
      [] -> putTextLn "No channel matched"
      xs -> do
        putTextLn $ "Sending notification to " <> show xs
        mapM_ (queueMessage changeEvent) xs
  _ -> pure mempty
  where
    queueMessage :: Gerrit.ChangeEvent -> Text -> IO ()
    queueMessage changeEvent roomId =
      atomically $ writeTBMQueue tqueue (toMatrixEvent roomId changeEvent)

-- | Group events by room id and author
-- See: https://hackage.haskell.org/package/relude-1.0.0.1/docs/Relude-Extra-Group.html#v:groupBy
groupEvents :: [MatrixEvent] -> [(Matrix.RoomID, Maybe Text, [Text])]
groupEvents events = fmap toGroup $ concat $ groupUserEvent <$> groupRoomEvents events
  where
    toGroup :: NonEmpty MatrixEvent -> (Matrix.RoomID, Maybe Text, [Text])
    toGroup (x :| xs) = (Matrix.RoomID (meRoomId x), meAuthor x, fmap meMessage (x : xs))
    groupUserEvent :: [MatrixEvent] -> [NonEmpty MatrixEvent]
    groupUserEvent = HM.elems . groupBy meAuthor
    groupRoomEvents :: [MatrixEvent] -> [[MatrixEvent]]
    groupRoomEvents = fmap toList . HM.elems . groupBy meRoomId

-- | The matrix client
sendEvents :: Matrix.Session -> [MatrixEvent] -> IO ()
sendEvents sess events = mapM_ send (groupEvents events)
  where
    send :: (Matrix.RoomID, Maybe Text, [Text]) -> IO ()
    send (roomID, authorM, messages) = do
      logMsg $ "Sending events " <> show events
      res <- Matrix.sendMessage sess roomID (formatMessages authorM messages)
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
