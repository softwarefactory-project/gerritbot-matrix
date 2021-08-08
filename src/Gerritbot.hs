{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A gerrit stream client
module Gerritbot (GerritServer (..), runStreamClient, isAlive) where

import qualified Control.Foldl as Fold
import Control.Monad.Catch (Handler (..))
import qualified Control.Retry as Retry
import qualified Data.Aeson as Aeson
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Gerrit.Event (Event, EventType, eventName)
import Gerritbot.Utils
import Relude
import qualified Turtle

data GerritServer = GerritServer
  { host :: Text,
    port :: Int,
    username :: Text
  }

resetAliveRef :: IORef (Maybe Int64) -> IO ()
resetAliveRef ref = do
  MkSystemTime now _ <- getSystemTime
  writeIORef ref (Just $ now + 2 + reconnectionDelay + connectionTimeout)

isAlive :: IORef (Maybe Int64) -> IO Bool
isAlive ref = do
  tsM <- readIORef ref
  case tsM of
    Nothing -> pure False
    Just ts -> do
      MkSystemTime now _ <- getSystemTime
      pure $ now > ts

reconnectionDelay, connectionTimeout :: Int64
reconnectionDelay = 1
connectionTimeout = 5

-- | 'runStreamClient' connects to a GerritServer with ssh and run the callback for each events.
runStreamClient ::
  Env ->
  GerritServer ->
  [EventType] ->
  (GerritServer -> Event -> IO ()) ->
  IO ()
runStreamClient env server@GerritServer {..} subscribeList cb = void loop
  where
    -- Recover from exception and reconnect after 1 second
    delayNano = fromInteger $ toInteger $ reconnectionDelay * 1_000_000
    loop = Retry.recovering (Retry.constantDelay delayNano) [\_ -> Handler handler] (const run)

    -- Handle client exit
    handler :: Turtle.ExitCode -> IO Bool
    handler ret = do
      let exitCode = case ret of
            Turtle.ExitSuccess -> 0
            Turtle.ExitFailure code -> code
      logErr $ SshExitCode exitCode
      env & logMetric $ SshRecon
      pure True

    -- Decode the event and callback
    onEvent evTxt = do
      env & logMetric $ GerritEventReceived
      case Aeson.decode $ encodeUtf8 evTxt of
        Just v -> cb server v
        Nothing -> logErr $ SshDecodeError evTxt

    run = do
      resetAliveRef (env & alive)
      logMsg $ Connecting host port
      Turtle.foldIO sshProc (Fold.mapM_ (onEvent . Turtle.lineToText))

    command = ["gerrit", "stream-events"] <> concatMap (\e -> ["-s", eventName e]) subscribeList
    sshCommand =
      ["-p", "29418"]
        <> ["-o", "ConnectTimeout=" <> show connectionTimeout]
        <> ["-o", "ServerAliveInterval=240"]
        <> ["-l", username, host]
        <> command
    sshProc = Turtle.inproc "ssh" sshCommand (pure mempty)
