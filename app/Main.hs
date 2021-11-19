-- | A silly module for the actual gerritbot-matrix executable.
module Main where

import qualified Gerritbot.Main
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 Gerritbot.Main.main
