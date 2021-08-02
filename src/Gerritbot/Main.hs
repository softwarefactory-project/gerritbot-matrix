{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Dhall hiding (maybe, void)
import qualified Dhall.Core
import qualified Dhall.Src
import qualified Dhall.TH
import qualified Gerrit.Event as Gerrit
import Gerritbot (GerritServer (..))
import qualified Gerritbot
import Gerritbot.Utils
import qualified Network.HTTP.Types.Status as HTTP
import Network.Matrix.Client (ClientSession, MatrixToken (..), RoomID (..))
import qualified Network.Matrix.Client as Matrix
import qualified Network.Matrix.Identity as Matrix
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Options.Generic
import qualified Prometheus
import Relude
import Relude.Extra.Group (groupBy)
import Say (sayErr)
import System.Directory (getModificationTime)

-- | Command line interface
-- See: http://hackage.haskell.org/package/optparse-generic-1.4.4/docs/Options-Generic.html
data CLI w = CLI
  { gerritHost :: w ::: Text <?> "The gerrit host",
    gerritUser :: w ::: Text <?> "The gerrit username",
    homeserverUrl :: w ::: Text <?> "The matrix homeserver url",
    identityUrl :: w ::: Maybe Text <?> "The matrix identity url",
    configFile :: w ::: FilePath <?> "The gerritbot.dhall path",
    monitoringPort :: w ::: Maybe Int <?> "HTTP listening port"
  }
  deriving stock (Generic)

instance ParseRecord (CLI Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

type RetryIO a = Matrix.MatrixIO a -> Matrix.MatrixIO a

-- | Generate Haskell Type from Dhall Type
-- See: https://hackage.haskell.org/package/dhall-1.38.0/docs/Dhall-TH.html
Dhall.TH.makeHaskellTypes
  [ Dhall.TH.MultipleConstructors "EventType" "(./src/Config.dhall).Event",
    Dhall.TH.SingleConstructor "Channel" "Channel" "(./src/Config.dhall).Type"
  ]

-- | Embed the expected configuration schema
configurationSchema :: Dhall.Core.Expr Dhall.Src.Src Void
configurationSchema = $(Dhall.TH.staticDhallExpression "(./src/Config.dhall).Type")

deriving instance Eq EventType

-- | Compare gerritbot event type config with original gerrit type
eventEquals :: Gerrit.EventType -> EventType -> Bool
eventEquals gerritEventType eventType = case (gerritEventType, eventType) of
  (Gerrit.PatchsetCreatedEvent, PatchsetCreated) -> True
  (Gerrit.ChangeMergedEvent, ChangeMerged) -> True
  _ -> False

data EventAction = EventAction
  { eaAuthor :: Text,
    eaAuthorMail :: Maybe Text,
    eaAction :: Doc
  }
  deriving (Show, Eq, Generic, Hashable)

newtype EventObject = EventObject {unObject :: Doc}
  deriving (Show, Eq)
  deriving newtype (Hashable)

data MatrixEvent = MatrixEvent
  { meAction :: EventAction,
    meObject :: EventObject,
    meRoomId :: RoomID,
    meTime :: Int64
  }
  deriving (Show, Eq)

-- | Prepare a Matrix Event
toMatrixEvent :: SystemTime -> Gerrit.Change -> Gerrit.User -> Gerrit.Event -> RoomID -> MatrixEvent
toMatrixEvent (MkSystemTime now _) Gerrit.Change {..} user event meRoomId = MatrixEvent {..}
  where
    meTime = fromMaybe now (Gerrit.getCreatedOn event)
    meAction = EventAction {..}
    eaAction = DocText $ " " <> verb <> ":"
    eaAuthorMail = Gerrit.userEmail user
    eaAuthor =
      fromMaybe
        "Anonymous User"
        (Gerrit.userName user <|> Gerrit.userUsername user <|> eaAuthorMail)
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

-- | Find if a channel match a change, return the roomId
getEventRoom :: GerritServer -> Gerrit.EventType -> Gerrit.Change -> (RoomID, Channel) -> Maybe RoomID
getEventRoom GerritServer {..} eventType Gerrit.Change {..} (roomId, Channel {..})
  | serverMatch && projectMatch && branchMatch && eventMatch = Just roomId
  | otherwise = Nothing
  where
    match eventValue confValue = glob (toString confValue) (toString eventValue)
    serverMatch = any (match host) servers
    projectMatch = any (match changeProject) projects
    branchMatch = any (match changeBranch) branches
    eventMatch = any (eventEquals eventType) events

-- | The gerritbot callback
onEvent :: IO [(RoomID, Channel)] -> TBMQueue MatrixEvent -> GerritServer -> Gerrit.Event -> IO ()
onEvent config tqueue server event =
  case (Gerrit.getChange event, Gerrit.getUser event) of
    (Just change, Just user) -> do
      channels <- config
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

data MatrixMessage = MatrixMessage
  { mmRoomID :: RoomID,
    mmEventAction :: EventAction,
    mmObjects :: NonEmpty EventObject,
    mmTS :: Int64,
    mmOldest :: Int64
  }
  deriving (Eq, Show)

-- | Group events by room id and author
-- See: https://hackage.haskell.org/package/relude-1.0.0.1/docs/Relude-Extra-Group.html#v:groupBy
groupEvents :: [MatrixEvent] -> [MatrixMessage]
groupEvents events =
  sortOn mmOldest $ fmap toMessage $ concat $ groupUserEvent <$> groupRoomEvents events
  where
    toMessage :: NonEmpty MatrixEvent -> MatrixMessage
    toMessage xs' =
      -- groupBy may have changed the order, thus we need to re-sort by the initial gerrit event timestamp.
      let xs = NE.sortWith meTime xs'
          (x :| _) = xs
          mmRoomID = meRoomId x
          mmEventAction = meAction x
          mmObjects = meObject <$> xs
          mmTS = mkTransactionId xs
          -- We also keep track of the oldest event for a given room so that message group can be sent in order.
          mmOldest = meTime x
       in MatrixMessage {..}
    --      (meRoomId x, meAction x, mkTransactionId (x : xs), fmap meObject (x : xs))
    mkTransactionId :: NonEmpty MatrixEvent -> Int64
    mkTransactionId = foldr (.|.) 0 . fmap meTime
    groupUserEvent :: [MatrixEvent] -> [NonEmpty MatrixEvent]
    groupUserEvent = HM.elems . groupBy meAction
    groupRoomEvents :: [MatrixEvent] -> [[MatrixEvent]]
    groupRoomEvents = fmap toList . HM.elems . groupBy meRoomId

-- | The matrix client
sendEvents ::
  -- | The matrix client session
  ClientSession ->
  RetryIO Matrix.EventID ->
  -- | A function to lookup identity
  (Text -> IO (Maybe Text)) ->
  -- | A list of event to proces
  [MatrixEvent] ->
  -- | The logMetric callback
  (MetricEvent -> IO ()) ->
  IO ()
sendEvents sess retry idLookup events logMetric = do
  logMsg $ "Sending events " <> show events
  traverse_ send (groupEvents events)
  where
    send :: MatrixMessage -> IO ()
    send (MatrixMessage roomID EventAction {..} messages ts _) = do
      -- Try to lookup matrix identity
      authorDoc <- case eaAuthorMail of
        Just mail -> do
          userIdM <- idLookup mail
          pure $ case userIdM of
            Just userId -> DocLink ("https://matrix.to/#/" <> userId) eaAuthor
            Nothing -> DocText eaAuthor
        Nothing -> pure $ DocText eaAuthor

      -- Format the room message
      let messageDoc = DocBody [authorDoc, eaAction, DocList . toList $ fmap unObject messages]
          mtBody = renderText messageDoc
          mtFormat = Just "org.matrix.custom.html"
          mtFormattedBody = Just $ renderHtml messageDoc
          txnId = toText . showDigest . sha1 . toLazy . encodeUtf8 $ mtBody <> show ts
          roomMessage = Matrix.RoomMessageNotice (Matrix.MessageText {..})

      -- Send the message
      res <- retry $ Matrix.sendMessage sess roomID (Matrix.EventRoomMessage roomMessage) (Matrix.TxnID txnId)
      logMetric MatrixMessageSent
      print res

-- | Sync the matrix client
doSyncClient :: ClientSession -> RetryIO RoomID -> [Channel] -> IO [RoomID]
doSyncClient sess retry = traverse joinRoom
  where
    -- TODO: leave room that are no longer configured
    joinRoom :: Channel -> IO RoomID
    joinRoom Channel {..} = do
      logMsg $ "Joining room " <> room
      eitherToError ("Failed to join " <> room)
        <$> retry (Matrix.joinRoom sess room)

-- | Creates an IO action that auto reload the config file if it changed
reloadConfig :: ClientSession -> RetryIO RoomID -> FilePath -> IO (IO [(RoomID, Channel)])
reloadConfig sess retry fp = do
  -- Get the current config
  configTS <- getModificationTime fp
  config <- load

  -- Create the reload action
  tsRef <- newIORef (configTS, config)
  pure (reload tsRef)
  where
    reload tsRef = do
      (prevConfigTS, prevConfig) <- readIORef tsRef
      configTS <- getModificationTime fp
      if configTS > prevConfigTS
        then do
          putTextLn $ toText fp <> ": reloading config"
          config <- load
          writeIORef tsRef (configTS, config)
          pure config
        else pure prevConfig

    load = do
      channels <- Dhall.input auto (toText fp)
      roomIds <- doSyncClient sess retry channels
      putTextLn $ "Joined: " <> show roomIds
      -- TODO: get all joined room and leave unknown room
      pure $ zip roomIds channels

-- | gerritbot-matrix entrypoint
main :: IO ()
main = do
  -- Handle special command line
  getArgs >>= \case
    ["print-config-schema"] -> do
      putTextLn $ Dhall.Core.pretty configurationSchema
      exitSuccess
    _anyOtherArgs -> pure ()

  -- Load the environment
  args <- unwrapRecord "Gerritbot Matrix"
  token <- fromMaybe (error "Missing MATRIX_TOKEN environment") <$> lookupEnv "MATRIX_TOKEN"
  idTokenM <- lookupEnv "MATRIX_IDENTITY_TOKEN"
  idPepperM <- lookupEnv "MATRIX_IDENTITY_PEPPER"

  -- Create http manager
  sess <- Matrix.createSession (homeserverUrl args) $! MatrixToken (toText token)
  idLookup <- case (identityUrl args, idTokenM, idPepperM) of
    (Just identityUrl, Just idToken, Just idPepper) -> do
      idSess <- Matrix.createIdentitySession identityUrl $! MatrixToken (toText idToken)
      pure $ mkIdLookup idSess (hashDetails $! idPepper)
    (Just _, _, _) ->
      error "Missing MATRIX_IDENTITY_TOKEN or MATRIX_IDENTITY_PEPPER environment"
    (Nothing, _, _) -> pure . const . pure $ Nothing

  -- Setup queue
  db <- dbNew
  tqueue <- newTBMQueueIO 2048

  -- Ssh connection monitor
  sshAliveRef <- newIORef False
  let gerritServer = Gerritbot.GerritServer (gerritHost args) 29418 (gerritUser args) sshAliveRef

  -- Spawn monitoring
  logMetric <- case monitoringPort args of
    Just port -> do
      void $ Async.async (Warp.run port $ monitoringApp sshAliveRef)
      logMetrics <$> registerMetrics
    Nothing -> pure $ const $ pure ()

  -- Network monitoring
  let logRetry err = do
        logMsg err
        logMetric HttpRetry
      retry = Matrix.retryWithLog 7 logRetry

  -- Load the config
  config <- reloadConfig sess retry (configFile args)

  -- Go!
  Async.concurrently_
    (runGerrit gerritServer (onEvent config tqueue) logMetric)
    (forever $ runMatrix sess retry (dbGet db idLookup) tqueue logMetric)
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

    -- TODO: fetch from server
    hashDetails = Matrix.HashDetails ("sha256" :| []) . toText
    mkIdLookup idSess hd email = do
      res <- Matrix.identityLookup idSess hd (Matrix.Email email)
      case res of
        Right (Just (Matrix.UserID x)) -> pure $ Just x
        Right Nothing -> pure Nothing
        Left e -> sayErr ("Lookup failed: " <> show e) >> pure Nothing

    runGerrit server cb =
      Gerritbot.runStreamClient server eventList cb

    runMatrix sess retry idLookup tqueue logMetric = do
      logMsg "Waiting for events"
      events <- bufferQueueRead 5_000_000 tqueue
      sendEvents sess retry idLookup events logMetric
      threadDelay 100_000

    monitoringApp aliveRef req resp = case Wai.rawPathInfo req of
      "/health" -> do
        alive <- readIORef aliveRef
        let status = if alive then HTTP.ok200 else HTTP.serviceUnavailable503
        resp $ Wai.responseLBS status [] mempty
      "/metrics" -> do
        metrics <- Prometheus.exportMetricsAsText
        resp $ Wai.responseLBS HTTP.ok200 [] metrics
      _anyOtherPath -> resp $ Wai.responseLBS HTTP.notFound404 [] mempty
