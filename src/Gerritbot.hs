{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A gerrit stream client
module Gerritbot (GerritServer (..), runStreamClient) where

import Control.Exception.Base (IOException)
import Control.Monad.Catch (Handler (..))
import Control.Monad.Trans.Except (throwE)
import qualified Control.Retry as Retry
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Gerrit.Event (Event, EventType, eventName)
import Gerritbot.Utils
import qualified Network.SSH.Client.LibSSH2 as SSH
import qualified Network.SSH.Client.LibSSH2.Errors as SSH
import qualified Network.SSH.Client.LibSSH2.Foreign as SSH
import Relude
import System.Environment (getEnv)

data GerritServer = GerritServer
  { host :: Text,
    port :: Int,
    username :: Text
  }

-- | 'runStreamClient' connects to a GerritServer with ssh and run the callback for each events.
runStreamClient ::
  Env ->
  GerritServer ->
  [EventType] ->
  (GerritServer -> Event -> IO ()) ->
  IO ()
runStreamClient env server@GerritServer {..} subscribeList cb = void loop
  where
    -- Decode a single event and callback
    onEvent ev = do
      env & logMetric $ GerritEventReceived
      case Aeson.decodeStrict ev of
        Just v -> cb server v
        Nothing -> logErr $ "Could not decode: " <> decodeUtf8 ev

    -- Process the events received
    onEvents = \case
      [] -> throwE "No events"
      -- The last event is not completed and we need to accumulate more data before processing.
      [x] -> pure x
      (x : xs) -> liftIO (onEvent x) >> onEvents xs

    -- Channel reading loop
    go ch acc = do
      eof <- liftIO $ SSH.channelIsEOF ch
      when eof $ do
        ret <- liftIO $ SSH.channelExitStatus ch
        throwE $ "Gerrit process exited: " <> show ret

      buf <- liftIO $ SSH.readChannel ch 1024
      when (BS.null buf) $
        throwE $ "Empty gerrit response, remaining " <> show acc

      let newLine = 10
      go ch =<< onEvents (BS.split newLine $ acc <> buf)

    -- Create the process and start the reading loop
    runChannel ch = do
      writeIORef (env & alive) True
      logMsg "Reading gerrit events stream..."
      SSH.channelExecute ch (toString $ unwords command)

      res <- runExceptT $ go ch ""
      case res of
        Left e -> logErr e
        Right () -> logErr "Stream silently failed"

      buf <- SSH.readChannelStderr ch 4096
      fail $
        if BS.null buf
          then "Empty error received"
          else "Gerrit error: " <> decodeUtf8 buf

    command = ["gerrit", "stream-events"] <> concatMap (\e -> ["-s", eventName e]) subscribeList

    -- Connect to the server
    connect = do
      home <- getEnv "HOME"
      let known_hosts = home <> "/.ssh/known_hosts"
          public = home <> "/.ssh/id_rsa.pub"
          private = home <> "/.ssh/id_rsa"
      logMsg $ "Connecting to " <> host <> ":" <> show port
      SSH.withSSH2
        known_hosts
        public
        private
        ""
        (toString username)
        (toString host)
        port
        (`SSH.withChannel` runChannel)

    -- Catch exception and retry after 1 second
    loop = Retry.recovering (Retry.constantDelay 1_000_000) handlers (const connect)
    handlers = [\_ -> Handler handlerSSH, \_ -> Handler handlerNetwork]

    -- Handle client exception
    handle inf e = do
      logErr $ inf <> " error: " <> show e
      writeIORef (env & alive) False
      env & logMetric $ SshRecon
      pure True
    handlerSSH :: SSH.ErrorCode -> IO Bool
    handlerSSH code = handle "ssh" code
    handlerNetwork :: IOException -> IO Bool
    handlerNetwork exc = handle "network" exc
