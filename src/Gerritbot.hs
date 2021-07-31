{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A gerrit stream client
module Gerritbot (GerritServer (..), runStreamClient, isAlive) where

import qualified Control.Foldl as Fold
import Control.Monad.Catch (Handler (..))
import qualified Control.Retry as Retry
import qualified Data.Aeson as Aeson
import Data.Text.IO (hPutStrLn)
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Gerrit.Event (Event, EventType, eventName)
import Gerritbot.Utils (MetricEvent (..))
import Relude
import qualified Turtle

err :: Text -> IO ()
err = hPutStrLn stderr . mappend "[E] "

data GerritServer = GerritServer
  { host :: Text,
    username :: Text,
    -- | aliveRef contains a timestamp after which the connection is considered alive
    aliveRef :: IORef (Maybe Int64)
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
  GerritServer ->
  [EventType] ->
  (GerritServer -> Event -> IO ()) ->
  (MetricEvent -> IO ()) ->
  IO ()
runStreamClient gerritServer subscribeList cb logMetric = loop
  where
    -- Recover from exception and reconnect after 1 second
    delayNano = fromInteger $ toInteger $ reconnectionDelay * 1_000_000
    loop = Retry.recovering (Retry.constantDelay delayNano) [\_ -> Handler handler] (const run)

    -- Handle client exit
    handler :: Turtle.ExitCode -> IO Bool
    handler code = do
      err $ "stream " <> show code
      logMetric SshRecon
      pure True

    -- Decode the event and callback
    onEvent evTxt = do
      logMetric GerritEventReceived
      case Aeson.decode $ encodeUtf8 evTxt of
        Just v -> cb gerritServer v
        Nothing -> err $ "Could not decode: " <> evTxt

    run = do
      resetAliveRef (aliveRef gerritServer)
      Turtle.foldIO sshProc (Fold.mapM_ (onEvent . Turtle.lineToText))

    command = ["gerrit", "stream-events"] <> concatMap (\e -> ["-s", eventName e]) subscribeList
    sshCommand =
      ["-p", "29418"]
        <> ["-o", "ConnectTimeout=" <> show connectionTimeout]
        <> ["-l", username gerritServer, host gerritServer]
        <> command
    sshProc = Turtle.inproc "ssh" sshCommand (pure mempty)
