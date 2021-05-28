{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The gerritbot-matrix main entrypoint
module Main where

import qualified Data.Text as Text
import Dhall
import qualified Dhall.TH
import qualified Gerrit.Event as Gerrit
import qualified Gerritbot
import qualified Matrix
import Options.Generic
import Relude

-- | Command line interface
-- See: http://hackage.haskell.org/package/optparse-generic-1.4.4/docs/Options-Generic.html
data CLI w = CLI
  { gerritHost :: w ::: Text <?> "The gerrit host",
    gerritUser :: w ::: Text <?> "The gerrit username",
    matrixUrl :: w ::: Text <?> "The matrix url",
    configFile :: w ::: FilePath <?> "The gerritbot.dhall path"
  }
  deriving stock (Generic)

instance ParseRecord (CLI Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

-- | Generate Haskell Type from Dhall Type
-- See: https://hackage.haskell.org/package/dhall-1.38.0/docs/Dhall-TH.html
Dhall.TH.makeHaskellTypes [Dhall.TH.SingleConstructor "Channel" "Channel" "(./src/Config.dhall).Type"]

-- | Match configuration glob with event value, only prefix glob is working at the moment
match :: Text -> Text -> Bool
match eventValue conf = conf `Text.isPrefixOf` eventValue

-- | Create text message for an event
formatMessage :: Gerrit.ChangeEvent -> Text
formatMessage Gerrit.ChangeEvent {..} =
  case changeEventType of
    Gerrit.PatchsetCreated -> author <> " proposed " <> info
    Gerrit.ChangeMerged -> "Merged " <> info
  where
    info = project <> " " <> branch <> ": " <> url
    url = Gerrit.changeUrl changeEventChange
    branch = Gerrit.changeBranch changeEventChange
    project = changeEventProject
    author = case changeEventUploader of
      Nothing -> "Unknown User"
      Just user ->
        fromMaybe
          "Anonymous User"
          (Gerrit.userName user <|> Gerrit.userUsername user <|> Gerrit.userEmail user)

-- | Find if a channel match an event, return the roomId
getEventRoom :: Gerrit.ChangeEvent -> Channel -> Maybe Text
getEventRoom Gerrit.ChangeEvent {..} Channel {..}
  | projectMatch && branchMatch = Just roomId
  | otherwise = Nothing
  where
    projectMatch = any (match changeEventProject) projects
    branchMatch = any (match $ Gerrit.changeBranch changeEventChange) branches

-- | The gerritbot callback
onEvent :: [Channel] -> Matrix.Session -> Gerrit.Event -> IO ()
onEvent channels sess event = case event of
  Gerrit.EventChange changeEvent@Gerrit.ChangeEvent {..} -> do
    putTextLn $ "Processing " <> show event
    case mapMaybe (getEventRoom changeEvent) channels of
      [] -> putTextLn "No channel matched"
      xs -> do
        putTextLn $ "Sending notification to " <> show xs
        mapM_ (sendMessage changeEvent) xs
  _ -> pure mempty
  where
    sendMessage :: Gerrit.ChangeEvent -> Text -> IO ()
    sendMessage changeEvent roomId = do
      res <- Matrix.sendMessage sess (Matrix.RoomID roomId) (formatMessage changeEvent)
      print res

-- | gerritbot-matrix entrypoint
main :: IO ()
main = do
  args <- unwrapRecord "Gerritbot Matrix"
  token <- fromMaybe (error "Missing MATRIX_TOKEN environment") <$> lookupEnv "MATRIX_TOKEN"
  sess <- Matrix.createSession (matrixUrl args) $! toText token
  channels <- Dhall.input auto (toText $ configFile args)
  Gerritbot.runStreamClient
    (Gerritbot.GerritServer (gerritHost args) (gerritUser args))
    (onEvent channels sess)
  putTextLn "Done."
