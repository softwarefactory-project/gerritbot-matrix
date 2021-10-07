{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Gerrit.Data.Event as Gerrit
import Gerritbot (GerritServer (..))
import Gerritbot.Main hiding (main)
import Gerritbot.Utils
import Relude
import Test.Hspec

main :: IO ()
main = hspec . spec $ GerritServer "" 29418 ""

spec :: GerritServer -> Spec
spec server = describe "unit tests" $ do
  it "match" $ do
    glob "software-factory/*" "software-factory/gerritbot-haskell"
      `shouldBe` True
  it "match config" $ do
    let room = "testRoom"
        projects = ["software-factory/*"]
        branches = ["main"]
        events = [PatchsetCreated]
        servers = ["*"]
        channel = Channel {..}
        change = fakeChange "software-factory/gerritbot-haskell" "main"
     in getEventRoom server Gerrit.PatchsetCreatedEvent change channel
          `shouldBe` Just "testRoom"
  it "group event" $ do
    let events =
          [ -- Sequencial events
            MatrixEvent action1 object1 room1 0,
            MatrixEvent action1 object3 room1 10,
            -- Interleaved events
            MatrixEvent action2 object1 room2 2,
            MatrixEvent action2 object1 room1 3,
            MatrixEvent action2 object2 room2 4,
            MatrixEvent action2 object2 room1 5,
            -- Out of order
            MatrixEvent action3 object2 room1 7,
            MatrixEvent action3 object1 room1 6,
            MatrixEvent action1 object2 room1 1,
            -- Different onBehalf
            MatrixEvent action4 object2 room1 100,
            MatrixEvent action5 object2 room1 101
          ]
     in groupEvents events
          `shouldBe` [ MatrixMessage room1 action1 (object1 :| [object2, object3]) 11 0,
                       MatrixMessage room2 action2 (object1 :| [object2]) 6 2,
                       MatrixMessage room1 action2 (object1 :| [object2]) 7 3,
                       MatrixMessage room1 action3 (object1 :| [object2]) 7 6,
                       MatrixMessage room1 action4 (object2 :| []) 100 100,
                       MatrixMessage room1 action5 (object2 :| []) 101 101
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
    behalf name = Just (MatrixAuthor name Nothing)
    author = MatrixAuthor "" Nothing
    (action1, action2, action3, action4, action5) =
      ( EventAction author Nothing "foo proposed:",
        EventAction author Nothing "foo merge:",
        EventAction author Nothing "bar do:",
        EventAction author (behalf "alice") "zuul merge:",
        EventAction author (behalf "bob") "zuul merge:"
      )
    (object1, object2, object3) =
      ( EventObject $ DocBody ["change1 ", DocLink "localhost" "title"],
        EventObject "change2",
        EventObject "change3"
      )
    (room1, room2) =
      ("room1", "room2")
    formatMessages (EventAction _ _ action) objects =
      DocBody [action, DocList $ fmap unObject objects]
    fakeUser :: Gerrit.User
    fakeUser = Gerrit.User (Just "tristan") Nothing Nothing
    fakeChange :: Text -> Text -> Gerrit.Change
    fakeChange changeProject changeBranch =
      let changeSubject = "DNM"
          changeUrl = "http://localhost"
          changeOwner = fakeUser
       in Gerrit.Change {..}
