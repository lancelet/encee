{-# LANGUAGE OverloadedStrings #-}

module Data.GCode.Simple.TokenizerSpec where

import           Data.GCode.Simple.Tokenizer (Pos (..), Token (..),
                                              TokenDetails (..),
                                              TokenizerError (..),
                                              TokenizerErrorType (..),
                                              byteStringSource, tokenize)

import qualified Data.ByteString.Lazy.Char8  as BS (unlines)
import           Test.Tasty                  (TestTree, testGroup)
import           Test.Tasty.HUnit            (testCase, (@=?))


tests :: TestTree
tests = testGroup "Data.GCode.Simple.Tokenizer tests"
  [ adHocTokenizeTest
  , adHocTokenizeSpaceTest
  , adHocTestUnknown
  , adHocTestNoClosingParen
  , adHocTestMissingRawFieldNumber
  ]


-- | Test tokenizing a small, ad-hoc test program.
adHocTokenizeTest :: TestTree
adHocTokenizeTest = testCase "tokenize (ad-hoc)" (expected @=? tokens)
  where
    tokens = tokenize (byteStringSource input)
    expected = Right <$>
      [ -- line 0
        Token (Pos 0  0) Percent
      , Token (Pos 0  1) (Newlines "\n")
        -- line 1
      , Token (Pos 1  0) (ParensComment "A small test program")
      , Token (Pos 1 22) (Newlines "\n")
        -- line 2
      , Token (Pos 2  0) (TokField 'n' "0010 ")
      , Token (Pos 2  6) (TokField 'G' "21 ")
      , Token (Pos 2 10) (TokField 'g' "0 ")
      , Token (Pos 2 13) (TokField 'x' "0 ")
      , Token (Pos 2 16) (TokField 'y' "0 ")
      , Token (Pos 2 19) (TokField 'z' "50 ")
      , Token (Pos 2 23) (SemicolonComment " top of part")
      , Token (Pos 2 36) (Newlines "\n")
        -- line 3
      , Token (Pos 3  0) BlockDelete
      , Token (Pos 3  1) (UnassignedWhitespace " ")
      , Token (Pos 3  2) (TokField 'n' "0190 ")
      , Token (Pos 3  8) (TokField 'g' "3 ")
      , Token (Pos 3 11) (TokField 'x' "13.5 ")
      , Token (Pos 3 17) (TokField 'y' "0 ")
      , Token (Pos 3 20) (TokField 'i' "-2.5")
      , Token (Pos 3 25) (Newlines "\n")
        -- line 4
      , Token (Pos 4  0) (TokField 'n' "0480 ")
      , Token (Pos 4  6) (ParensComment "a comment")
      , Token (Pos 4 17) (UnassignedWhitespace " ")
      , Token (Pos 4 18) (TokField 'y' "4 ")
      , Token (Pos 4 21) (TokField '*' "42")
      , Token (Pos 4 24) (Newlines "\n")
      ]
    input = BS.unlines
      [ "%"
      , "(A small test program)"
      , "n0010 G21 g0 x0 y0 z50 ; top of part"
      , "/ n0190 g3 x13.5 y0 i-2.5"
      , "n0480 (a comment) y4 *42"
      ]


-- | Test unusual, but permitted, spacing in the input.
--   From:
--     Kramer et al. (2000) The NIST RS274NGC Interpreter - Version 3.
--     Section 3.3
adHocTokenizeSpaceTest :: TestTree
adHocTokenizeSpaceTest
  = testCase
    "tokenize input with unusual spacing (ad-hoc)"
    (expected @=? tokens)
  where
    expected = Right <$>
      [ Token (Pos 0  0) (TokField 'g' "0")
      , Token (Pos 0  2) (TokField 'x' " +0. 12 34")
      , Token (Pos 0 13) (TokField 'y' " 7")
      ]
    tokens = tokenize (byteStringSource input)
    input = "g0x +0. 12 34y 7"


-- | Test an unknown character.
adHocTestUnknown :: TestTree
adHocTestUnknown
  = testCase "tokenize unknown character error (ad-hoc)" (expected @=? tokens)
  where
    expected = [ Left $ TokenizerError UnknownChar (Pos 0 0) ]
    tokens = tokenize (byteStringSource input)
    input = "[ 1 + acos[0] ]"


-- | Test a comment without a closing parenthesis.
adHocTestNoClosingParen :: TestTree
adHocTestNoClosingParen
  = testCase
    "tokenizing a parens comment without closing parens error (ad-hoc)"
    (expected @=? tokens)
  where
    expected = [ Left $ TokenizerError NoClosingParen (Pos 0 0) ]
    tokens = tokenize (byteStringSource input)
    input = "(bad comment"


-- | Test a field without any number.
adHocTestMissingRawFieldNumber :: TestTree
adHocTestMissingRawFieldNumber
  = testCase
    "tokenizing a field missing a number produces an error (ad-hoc)"
    (expected @=? tokens)
  where
    expected = [ Left $ TokenizerError MissingRawFieldNumber (Pos 0 0) ]
    tokens = tokenize (byteStringSource input)
    input = "GX"
