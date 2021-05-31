{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Gerrit.Event as Gerrit
import Gerritbot (GerritServer (..))
import Gerritbot.Main hiding (main)
import Gerritbot.Utils
import Matrix (RoomID (..))
import Relude
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "unit tests" $ do
  it "match" $ do
    glob "software-factory/*" "software-factory/gerritbot-haskell"
      `shouldBe` True
  it "match config" $ do
    let roomId = "testRoom"
        projects = ["software-factory/*"]
        branches = ["main"]
        events = [PatchsetCreated]
        servers = ["*"]
        channel = Channel {..}
        change = fakeChange "software-factory/gerritbot-haskell" "main"
     in getEventRoom (GerritServer "" "") Gerrit.PatchsetCreatedEvent change channel `shouldBe` Just (RoomID "testRoom")
  it "group event" $ do
    let events =
          [ -- Sequencial events
            MatrixEvent action1 object1 room1 0,
            MatrixEvent action1 object2 room1 0,
            -- Interleaved events
            MatrixEvent action2 object1 room2 0,
            MatrixEvent action2 object1 room1 0,
            MatrixEvent action2 object2 room2 0,
            MatrixEvent action2 object2 room1 0,
            -- Out of order
            MatrixEvent action3 object2 room1 0,
            MatrixEvent action3 object1 room1 0
          ]
     in groupEvents events
          `shouldBe` [ (room1, action2, 0, [object1, object2]),
                       (room1, action1, 0, [object1, object2]),
                       (room1, action3, 0, [object2, object1]),
                       (room2, action2, 0, [object1, object2])
                     ]
  it "format oneline" $ do
    renderText (formatMessages action1 [object1])
      `shouldBe` "foo proposed: change1 title  localhost"
  it "format multiline" $ do
    renderText (formatMessages action1 [object1, object2])
      `shouldBe` "foo proposed:\n- change1 title  localhost\n- change2"
  it "format html oneline" $ do
    renderHtml (formatMessages action1 [object1])
      `shouldBe` "foo proposed: change1 <a href=\"localhost\">title</a>"
  it "format html multiline" $ do
    renderHtml (formatMessages action1 [object1, object2])
      `shouldBe` "foo proposed:\n<ul><li>change1 <a href=\"localhost\">title</a></li><li>change2</li></ul>"
  where
    (action1, action2, action3) =
      ( EventAction "foo proposed:",
        EventAction "foo merge:",
        EventAction "bar do:"
      )
    (object1, object2) =
      (EventObject $ DocBody ["change1 ", DocLink "localhost" "title"], EventObject "change2")
    (room1, room2) =
      (RoomID "room1", RoomID "room2")
    fakeUser :: Gerrit.User
    fakeUser = Gerrit.User (Just "tristan") Nothing Nothing
    fakeChange :: Text -> Text -> Gerrit.Change
    fakeChange changeProject changeBranch =
      let changeSubject = "DNM"
          changeUrl = "http://localhost"
          changeOwner = fakeUser
       in Gerrit.Change {..}
