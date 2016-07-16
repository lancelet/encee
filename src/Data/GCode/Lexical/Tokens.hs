module Data.GCode.Lexical.Tokens where

import           Data.Scientific (Scientific)
import           Data.Text       (Text)

data Token
  =
    --
    -- Delimiters
    --
    Tok_LBracket                -- ^ [
  | Tok_RBracket                -- ^ ]

    --
    -- Reserved Identifiers
    --
  | Tok_SetVn                   -- ^ setvn
  | Tok_If                      -- ^ if
  | Tok_Then                    -- ^ then
  | Tok_Goto                    -- ^ goto
  | Tok_While                   -- ^ while
  | Tok_Do                      -- ^ do
  | Tok_End                     -- ^ end
  | Tok_Hash                    -- ^ #

    --
    -- Identifiers
    --
  | Tok_Id !Text                -- ^ word, variable or function identifier

    --
    -- Operators
    --
  | Tok_Op !Char                -- ^ plus, minus, multiply, divide, etc.

    --
    -- Literals
    --
  | Tok_LitNumber !Scientific   -- ^ literal number

    --
    -- Comments
    --
  | Tok_ParenComment !Text      -- ^ parenthesis-delimited comment
  | Tok_SemiComment !Text       -- ^ semicolon-EOL-delimited comment

  deriving (Eq, Show)
