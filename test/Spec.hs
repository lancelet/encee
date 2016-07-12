{-# OPTIONS_GHC -Wwarn #-}

module Main
  ( main
  ) where

import qualified Data.GCode.Simple.TokenizerSpec as Tokenizer (tests)

import           Test.DocTest                    (doctest)
import           Test.Tasty                      (TestTree, defaultMain,
                                                  testGroup)

main :: IO ()
main =
  do
    doctest doctests
    defaultMain tests

tests :: TestTree
tests = testGroup "Testy tests"
  [ Tokenizer.tests
  ]


doctests :: [String]
doctests =
  [ "src/Data/GCode/Simple/Tokenizer.hs"
  ]
