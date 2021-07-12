{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Utility function
module Gerritbot.Utils where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import qualified Control.Concurrent.STM.TBMQueue as TBMQueue
import Data.Time.Clock (getCurrentTime)
import Relude
import Say

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

logMsg :: Text -> IO ()
logMsg msg = do
  now <- getCurrentTime
  th <- myThreadId
  say $ show now <> " [" <> show th <> "]: " <> msg

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
