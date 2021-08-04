{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Utility function
module Gerritbot.Utils where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import qualified Control.Concurrent.STM.TBMQueue as TBMQueue
import Data.Fixed (Pico)
import qualified Data.HashTable.IO as HT
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Prometheus (Counter)
import qualified Prometheus
import Prometheus.Metric.GHC (ghcMetrics)
import Relude
import Say

data Env = Env
  { logMetric :: MetricEvent -> IO (),
    alive :: IORef Bool
  }

data MetricEvent = SshRecon | HttpRetry | GerritEventReceived | MatrixMessageSent

logMetrics :: Metrics -> MetricEvent -> IO ()
logMetrics Metrics {..} ev = case ev of
  SshRecon -> Prometheus.incCounter sshRecon
  HttpRetry -> Prometheus.incCounter httpRetry
  GerritEventReceived -> Prometheus.incCounter gerritEvents
  MatrixMessageSent -> Prometheus.incCounter matrixMessages

data Metrics = Metrics
  { sshRecon :: Counter,
    httpRetry :: Counter,
    gerritEvents :: Counter,
    matrixMessages :: Counter
  }

registerMetrics :: IO Metrics
registerMetrics = do
  void $ Prometheus.register ghcMetrics
  Metrics
    <$> mkCounter "ssh_errors" "Gerrit reconnection attempts"
    <*> mkCounter "http_errors" "Http connection errors"
    <*> mkCounter "gerrit_events" "Gerrit events received"
    <*> mkCounter "matrix_messages" "Matrix messages sent"
  where
    mkCounter name desc =
      Prometheus.register $ Prometheus.counter (Prometheus.Info name desc)

-------------------------------------------------------------------------------
-- A refreshable database, to store roomID and identity lookup result

-- | 'DB k v' is a mutable hashtable of key 'k' and value 'v' type.
--   The value is either a failure timestamp or the actual value.
newtype DB k v = DB (HT.BasicHashTable k (Either UTCTime v))

dbNew :: IO (DB k v)
dbNew = DB <$> HT.new

dbGet :: (Hashable k, Eq k) => DB k v -> Pico -> (k -> IO (Maybe v)) -> k -> IO (Maybe v)
dbGet (DB db) delay fetch key = do
  now <- getCurrentTime
  valueME <- HT.lookup db key
  case valueME of
    Just (Right value) -> pure $ Just value
    Just (Left date) -> do
      if nominalDiffTimeToSeconds (diffUTCTime now date) > delay
        then doFetch now
        else pure Nothing
    Nothing -> doFetch now
  where
    doFetch now = do
      valueM <- fetch key
      let value = case valueM of
            Just x -> Right x
            Nothing -> Left now
      HT.insert db key value
      pure valueM

-------------------------------------------------------------------------------
-- Utilities

-- | Globing
-- >>> glob "" ""
-- True
-- >>> map (flip glob "hello") ["h??lo", "h*o", "h*ello", "*hello*"]
-- [True,True,True,True]
-- >>> map (flip glob "hello wolrd") ["*h*o*", "h*o*", "*h*d", "*h*l*w*d", "*h?l*w*d"]
-- [True,True,True,True,True]
-- >>> glob "hello" "hi"
-- False
-- >>> glob "h?i" "hi"
-- False
-- >>> glob "h*l" "hello"
-- False
glob :: String -> String -> Bool
glob ('*' : ps) xs = wildcardMatch ps xs
  where
    wildcardMatch (p' : ps') (x' : xs') = glob (if p' == x' then ps' else '*' : ps) xs'
    wildcardMatch [] _ = True
    wildcardMatch _ _ = False
glob ('?' : ps) (_ : xs) = glob ps xs
glob (p : ps) (x : xs) = p == x && glob ps xs
glob [] [] = True
glob _ _ = False

doLog :: Handle -> Text -> IO ()
doLog handle msg = do
  now <- getCurrentTime
  th <- myThreadId
  hSay handle $ Text.take 23 (show now) <> " [" <> show th <> "]: " <> msg
  hFlush handle

logMsg, logErr :: Text -> IO ()
logMsg = doLog stdout
logErr = doLog stderr

eitherToError :: Show a => Text -> Either a b -> b
eitherToError msg x = case x of
  Left err -> error $ msg <> ": " <> show err
  Right b -> b

-- | Helper function to group queue event based on a time limit
bufferQueueRead :: Int -> TBMQueue a -> IO [a]
bufferQueueRead maxTime tqueue = do
  event <- fromMaybe (error "Queue is closed") <$> atomically (TBMQueue.readTBMQueue tqueue)
  logMsg "Got one event, now buffering"
  threadDelay maxTime
  atomically $ drainQueue [event]
  where
    drainQueue acc = do
      event <- fromMaybe (error "Queue is closed") <$> TBMQueue.tryReadTBMQueue tqueue
      case event of
        Nothing -> pure $ reverse acc
        Just ev -> drainQueue (ev : acc)

-- | Message formatting
data Doc
  = DocBody [Doc]
  | DocText Text
  | DocLink Text Text
  | DocList [Doc]
  deriving (Show, Eq, Generic, Hashable)

instance IsString Doc where
  fromString = DocText . toText

renderText :: Doc -> Text
renderText doc = case doc of
  DocBody xs -> mconcat (fmap renderText xs)
  DocText x -> x
  DocLink url name -> name <> "  " <> url
  DocList [x] -> " " <> renderText x
  DocList xs -> mconcat $ fmap renderItem xs
  where
    renderItem x = "\n- " <> renderText x

renderHtml :: Doc -> Text
renderHtml doc = case doc of
  DocBody xs -> mconcat (fmap renderHtml xs)
  DocText x -> x
  DocLink url name -> "<a href=\"" <> url <> "\">" <> name <> "</a>"
  DocList [x] -> " " <> renderHtml x
  DocList xs -> "\n<ul>" <> mconcat (fmap renderItem xs) <> "</ul>"
  where
    renderItem x = "<li>" <> renderHtml x <> "</li>"
