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
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Dhall hiding (maybe, void)
import qualified Dhall.Core
import qualified Dhall.Src
import qualified Dhall.TH
import qualified Gerrit.Event as Gerrit
import Gerritbot (GerritServer (..))
import qualified Gerritbot
import Gerritbot.Utils
import qualified Network.HTTP.Client as HTTP
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

-------------------------------------------------------------------------------
-- Convert initial gerrit event to a matrix event

-- | Compare gerritbot event type config with original gerrit type
eventEquals :: Gerrit.EventType -> EventType -> Bool
eventEquals gerritEventType eventType = case (gerritEventType, eventType) of
  (Gerrit.PatchsetCreatedEvent, PatchsetCreated) -> True
  (Gerrit.ChangeMergedEvent, ChangeMerged) -> True
  _ -> False

data MatrixAuthor = MatrixAuthor
  { maName :: Text,
    maMail :: Maybe Text
  }
  deriving (Show, Eq, Generic, Hashable)

data EventAction = EventAction
  { eaAuthor :: MatrixAuthor,
    eaOnBehalf :: Maybe MatrixAuthor,
    eaAction :: Doc
  }
  deriving (Show, Eq, Generic, Hashable)

newtype EventObject = EventObject {unObject :: Doc}
  deriving (Show, Eq)
  deriving newtype (Hashable)

data MatrixEvent = MatrixEvent
  { meAction :: EventAction,
    meObject :: EventObject,
    meRoom :: Text,
    meTime :: Int64
  }
  deriving (Show, Eq)

-- | Prepare a Matrix Event
toMatrixEvent :: SystemTime -> Gerrit.Change -> Gerrit.User -> Gerrit.Event -> Text -> MatrixEvent
toMatrixEvent (MkSystemTime now _) Gerrit.Change {..} user event meRoom = MatrixEvent {..}
  where
    meTime = fromMaybe now (Gerrit.getCreatedOn event)
    meAction = EventAction {..}
    eaAction = DocText $ " " <> verb <> ":"
    eaOnBehalf
      | Gerrit.userEmail changeOwner /= Gerrit.userEmail user = meAuthorM
      | otherwise = Nothing
    getAuthor user' =
      fromMaybe
        "Anonymous User"
        (Gerrit.userName user' <|> Gerrit.userUsername user' <|> Gerrit.userEmail user')
    meAuthorM = Just (MatrixAuthor (getAuthor changeOwner) (Gerrit.userEmail changeOwner))
    eaAuthor = MatrixAuthor (getAuthor user) (Gerrit.userEmail user)
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

-- | Find if a channel match a change, return the room name
getEventRoom :: GerritServer -> Gerrit.EventType -> Gerrit.Change -> Channel -> Maybe Text
getEventRoom GerritServer {..} eventType Gerrit.Change {..} Channel {..}
  | serverMatch && projectMatch && branchMatch && eventMatch = Just room
  | otherwise = Nothing
  where
    match eventValue confValue = glob (toString confValue) (toString eventValue)
    serverMatch = any (match host) servers
    projectMatch = any (match changeProject) projects
    branchMatch = any (match changeBranch) branches
    eventMatch = any (eventEquals eventType) events

-- | The gerritbot callback
onEvent :: IO [Channel] -> TBMQueue MatrixEvent -> GerritServer -> Gerrit.Event -> IO ()
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
  { mmRoom :: Text,
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
          mmRoom = meRoom x
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
    groupRoomEvents = fmap toList . HM.elems . groupBy meRoom

-- | The matrix client
sendEvents ::
  Env ->
  -- | The matrix client session
  ClientSession ->
  -- | A function to lookup identity
  (Text -> IO (Maybe Text)) ->
  -- | A function to join and get roomID
  (Text -> IO (Maybe RoomID)) ->
  -- | A list of event to proces
  [MatrixEvent] ->
  IO ()
sendEvents env sess idLookup joinRoom events = do
  -- logMsg $ "Sending events " <> show events
  traverse_ send (groupEvents events)
  where
    logRetry err = do
      logErr $ HttpError err
      env & logMetric $ HttpRetry
    retry = Matrix.retryWithLog 7 logRetry
    send :: MatrixMessage -> IO ()
    send (MatrixMessage room EventAction {..} messages ts _) = do
      -- Try to lookup matrix identity
      let lookupId MatrixAuthor {..} = case maMail of
            Just mail -> do
              userIdM <- idLookup mail
              pure $ case userIdM of
                Just userId -> DocLink ("https://matrix.to/#/" <> userId) maName
                Nothing -> DocText maName
            Nothing -> pure $ DocText maName

      authorDoc <- lookupId eaAuthor
      -- Format the room message
      let messageDoc = DocBody [authorDoc, eaAction, DocList . toList $ fmap unObject messages]
          mtBody = renderText messageDoc
          mtType = Matrix.NoticeType
          mtFormat = Just "org.matrix.custom.html"
          mtFormattedBody = Just $ renderHtml messageDoc
          txnId = toText . showDigest . sha1 . toLazy . encodeUtf8 $ mtBody <> show ts
          roomMessage = Matrix.RoomMessageText (Matrix.MessageText {..})

      -- Send the message
      roomIDM <- joinRoom room
      case roomIDM of
        Just roomID -> do
          res <- retry $ Matrix.sendMessage sess roomID (Matrix.EventRoomMessage roomMessage) (Matrix.TxnID txnId)
          env & logMetric $ MatrixMessageSent
          print res
        Nothing -> logErr $ MatrixPostError (show roomMessage) room

-- | Sync the matrix client
doSyncClient :: ClientSession -> [Channel] -> IO [(RoomID, Channel)]
doSyncClient sess channels = do
  -- Join unique channels
  let roomNames = nub $ fmap room channels
  roomIds <- traverse joinRoom roomNames
  putTextLn $ "Joined: " <> show roomIds

  -- TODO: get all joined room and leave unknown room
  -- joinedRooms <- Matrix.getJoinedRooms sess

  let roomNamesIds = Map.fromList $ zip roomNames roomIds
      getRoomID channel =
        fromMaybe (error $ "Uknown room: " <> room channel) $
          Map.lookup (room channel) roomNamesIds
  pure $ fmap (\channel -> (getRoomID channel, channel)) channels
  where
    joinRoom :: Text -> IO RoomID
    joinRoom roomName = do
      putTextLn $ "Joining room " <> roomName
      eitherToError ("Failed to join " <> roomName)
        <$> Matrix.retry (Matrix.joinRoom sess roomName)

-- | Creates an IO action that auto reload the config file if it changed
reloadConfig :: FilePath -> IO (IO [Channel])
reloadConfig fp = do
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

    load = Dhall.input auto (toText fp)

-- | gerritbot-matrix entrypoint
main :: IO ()
main = do
  -- Handle special command line
  getArgs >>= \case
    ["print-config-schema"] -> do
      putTextLn $ Dhall.Core.pretty configurationSchema
      exitSuccess
    ["check", port] -> do
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      request <- HTTP.parseUrlThrow $ "http://127.0.0.1:" <> port <> "/health"
      void $ HTTP.httpLbs request manager
      putTextLn "Success!"
      exitSuccess
    _anyOtherArgs -> pure ()

  args <- unwrapRecord "Gerritbot Matrix"
  labelMyThread "Main"

  -- Setup monitoring
  alive <- newIORef Nothing
  env <- case monitoringPort args of
    Just port -> do
      logMetric <- logMetrics <$> registerMetrics
      let env = Env logMetric alive
      void $ Async.async (Warp.run port $ monitoringApp env)
      pure env
    Nothing -> pure $ Env (const $ pure ()) alive

  mainBot args env

  logErr BotExit
  exitFailure

mainBot :: CLI Unwrapped -> Env -> IO ()
mainBot args env = do
  -- Load the environment
  token <- fromMaybe (error "Missing MATRIX_TOKEN environment") <$> lookupEnv "MATRIX_TOKEN"
  idTokenM <- lookupEnv "MATRIX_IDENTITY_TOKEN"

  -- Create http manager
  sess <- Matrix.createSession (homeserverUrl args) $! MatrixToken (toText token)
  validateSession sess

  -- Setup room join function
  joinRoom <- do
    roomDB <- dbNew
    pure $ dbGet roomDB 60 $ mkJoinRoom sess

  -- Setup identity lookup function
  idLookup <- case (identityUrl args, idTokenM) of
    (Just identityUrl, Just idToken) | idToken /= mempty -> do
      idDB <- dbNew
      idSess <- Matrix.createIdentitySession identityUrl $! MatrixToken (toText idToken)
      hdE <- Matrix.hashDetails idSess
      case hdE of
        Left err -> fail $ "Could not get hash details: " <> show err
        Right hd -> pure $ dbGet idDB 600 $ mkIdLookup idSess hd
    (Just _, _) ->
      error "Missing MATRIX_IDENTITY_TOKEN environment"
    (Nothing, _) -> pure . const . pure $ Nothing

  -- Setup events queue
  tqueue <- newTBMQueueIO 2048

  -- Load the config
  config <- reloadConfig $ configFile args

  -- Go!
  Async.race_
    (labelMyThread "Gerrit" >> runGerrit (onEvent config tqueue))
    (labelMyThread "Matrix" >> runMatrix tqueue (sendEvents env sess idLookup joinRoom))
  where
    -- TODO: infer subscribe list from channels configuration
    eventList =
      [ Gerrit.ChangeAbandonedEvent,
        Gerrit.ChangeDeletedEvent,
        Gerrit.ChangeMergedEvent,
        Gerrit.ChangeRestoredEvent,
        Gerrit.PatchsetCreatedEvent
      ]

    mkIdLookup idSess hd email = do
      res <- Matrix.identityLookup idSess hd (Matrix.Email email)
      case res of
        Right (Just (Matrix.UserID x)) -> pure $ Just x
        Right Nothing -> pure Nothing
        Left e -> do
          logErr $ MatrixLookupFail (show e)
          env & logMetric $ HttpRetry
          pure Nothing

    mkJoinRoom sess room = do
      roomIDE <- Matrix.retry $ Matrix.joinRoom sess room
      case roomIDE of
        Left err -> do
          logErr $ MatrixJoinError (show err) room
          env & logMetric $ HttpRetry
          pure Nothing
        Right roomID -> do
          logMsg $ JoinedRoom room roomID
          pure $ Just roomID

    validateSession sess = do
      ownerE <- Matrix.retry $ Matrix.getTokenOwner sess
      case ownerE of
        Right owner -> logMsg $ MatrixLoggedIn owner
        Left err -> fail $ "Invalid matrix token: " <> show err

    runGerrit cb =
      let server = Gerritbot.GerritServer (gerritHost args) 29418 (gerritUser args)
       in Gerritbot.runStreamClient env server eventList cb

    runMatrix tqueue cb = forever $ do
      logMsg MatrixReady
      events <- bufferQueueRead 5_000_000 tqueue
      void $ cb events
      threadDelay 100_000

monitoringApp :: Env -> Wai.Request -> (Wai.Response -> IO a) -> IO a
monitoringApp env req resp = case Wai.rawPathInfo req of
  "/health" -> do
    alive <- Gerritbot.isAlive (env & alive)
    let status = if alive then HTTP.ok200 else HTTP.serviceUnavailable503
    resp $ Wai.responseLBS status [] mempty
  "/metrics" -> do
    metrics <- Prometheus.exportMetricsAsText
    resp $ Wai.responseLBS HTTP.ok200 [] metrics
  _anyOtherPath -> resp $ Wai.responseLBS HTTP.notFound404 [] mempty
