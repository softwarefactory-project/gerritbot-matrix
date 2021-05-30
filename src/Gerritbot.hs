{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A gerrit stream client
module Gerritbot (GerritServer (..), runStreamClient) where

import qualified Control.Foldl as Fold
import Control.Monad.Catch (Handler (..))
import qualified Control.Retry as Retry
import qualified Data.Aeson as Aeson
import Data.Text.IO (hPutStrLn)
import Gerrit.Event (Event, EventType, eventName)
import Relude
import qualified Turtle

err :: Text -> IO ()
err = hPutStrLn stderr . mappend "[E] "

data GerritServer = GerritServer
  { host :: Text,
    username :: Text
  }

-- | 'runStreamClient' connects to a GerritServer with ssh and run the callback for each events.
runStreamClient :: GerritServer -> [EventType] -> (GerritServer -> Event -> IO ()) -> IO ()
runStreamClient gerritServer subscribeList cb = loop
  where
    -- Recover from exception and reconnect after 1 second
    loop = Retry.recovering (Retry.constantDelay 1000000) [\_ -> Handler handler] (const run)
    -- Handle client exit
    handler :: Turtle.ExitCode -> IO Bool
    handler code = do
      err $ "stream " <> show code
      pure True
    -- Decode the event and callback
    onEvent evTxt = do
      case Aeson.decode $ encodeUtf8 evTxt of
        Just v -> cb gerritServer v
        Nothing -> err $ "Could not decode: " <> evTxt
    run = Turtle.foldIO sshProc (Fold.mapM_ (onEvent . Turtle.lineToText))
    command = ["gerrit", "stream-events"] <> concatMap (\e -> ["-s", eventName e]) subscribeList
    sshCommand = ["-p", "29418", "-l", username gerritServer, host gerritServer] <> command
    sshProc = Turtle.inproc "ssh" sshCommand (pure mempty)
