{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

log :: Text -> IO ()
log = putTextLn . mappend "[+] "

data GerritServer = GerritServer
  { host :: Text,
    username :: Text
  }

runStreamClient :: GerritServer -> (Event -> IO ()) -> IO ()
runStreamClient gerritServer cb = loop
  where
    loop = Retry.recovering (Retry.constantDelay 1000000) [\_ -> Handler handler] (const run)
    handler :: Turtle.ExitCode -> IO Bool
    handler code = do
      err $ "stream " <> show code
      pure True
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
