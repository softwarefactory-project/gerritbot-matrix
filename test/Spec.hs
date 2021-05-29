{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Gerrit.Event as Gerrit
import Gerritbot.Main (Channel (..), getEventRoom, match)
import Relude
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "unit tests" $ do
  it "match" $ do
    match "software-factory/gerritbot-haskell" "software-factory/"
      `shouldBe` True
  it "match config" $ do
    let roomId = "testRoom"
        projects = ["software-factory/"]
        branches = ["main"]
        channel = Channel {..}
        event = fakeEvent "software-factory/gerritbot-haskell" "main"
     in getEventRoom event channel `shouldBe` Just "testRoom"
  where
    fakeUser :: Gerrit.User
    fakeUser = Gerrit.User (Just "tristan") Nothing Nothing
    fakeChange :: Text -> Text -> Gerrit.Change
    fakeChange changeProject changeBranch =
      let changeSubject = "DNM"
          changeUrl = "http://localhost"
          changeOwner = fakeUser
       in Gerrit.Change {..}
    fakeEvent :: Text -> Text -> Gerrit.ChangeEvent
    fakeEvent changeEventProject branch =
      let changeEventType = Gerrit.PatchsetCreated
          changeEventUploader = Nothing
          changeEventChange = fakeChange changeEventProject branch
          changeEventPatchSet = Gerrit.PatchSet 0 fakeUser
       in Gerrit.ChangeEvent {..}
