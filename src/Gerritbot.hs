{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A gerrit stream client
module Gerritbot (GerritServer (..), runStreamClient) where

import qualified Control.Foldl as Fold
import Control.Monad.Catch (Handler (..))
import qualified Control.Retry as Retry
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Text.IO (hPutStrLn)
import Gerrit.Event (Event)
import Relude
import qualified Turtle

err :: Text -> IO ()
err = hPutStrLn stderr . mappend "[E] "

data GerritServer = GerritServer
  { host :: Text,
    username :: Text
  }

-- | 'runStreamClient' connects to a GerritServer with ssh and run the callback for each events.
runStreamClient :: GerritServer -> (Event -> IO ()) -> IO ()
runStreamClient gerritServer cb = loop
  where
    -- Recover from exception and reconnect after 1 second
    loop = Retry.recovering (Retry.constantDelay 1000000) [\_ -> Handler handler] (const run)
    -- Handle client exit
    handler :: Turtle.ExitCode -> IO Bool
    handler code = do
      err $ "stream " <> show code
      pure True
    -- Decode the event and skip ref-replicated
    onEvent evTxt = do
      case Aeson.decode $ encodeUtf8 evTxt of
        Just v -> cb v
        Nothing ->
          if Text.isInfixOf "ref-replicat" evTxt
            then pure ()
            else err $ "Could not decode: " <> evTxt
    run = Turtle.foldIO sshProc (Fold.mapM_ (onEvent . Turtle.lineToText))
    sshProc =
      Turtle.inproc
        "ssh"
        ["-p", "29418", "-l", username gerritServer, host gerritServer, "gerrit", "stream-events"]
        (pure mempty)
