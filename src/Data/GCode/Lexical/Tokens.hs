{-# LANGUAGE OverloadedStrings #-}

module Data.GCode.Lexical.Tokens where

import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import qualified Data.Text       as T (concat, pack, singleton)

data Token
  =
    --
    -- Delimiters
    --
    Tok_LBracket                -- ^ [
  | Tok_RBracket                -- ^ ]

    --
    -- Identifiers
    --
  | Tok_Id !Identifier

    --
    -- Operators
    --
  | Tok_Op !Char                -- ^ plus, minus, multiply, divide, hash, etc.

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

data Identifier
  = Id_Name !Text
  | Id_Reserved !ReservedId
  deriving (Eq, Show)

data ReservedId
  = RId_SetVn
  | RId_If
  | RId_Then
  | RId_Goto
  | RId_While
  | RId_Do
  | RId_End
  deriving (Eq, Show)

renderToken :: Token -> Text
renderToken t =
  case t of
    Tok_LBracket       -> "["
    Tok_RBracket       -> "]"
    Tok_Id x           -> renderId x
    Tok_Op c           -> T.singleton c
    Tok_LitNumber n    -> T.pack . show $ n
    Tok_ParenComment c -> T.concat ["(", c, ")"]
    Tok_SemiComment c  -> T.concat [";", c]

renderId :: Identifier -> Text
renderId x =
  case x of
    Id_Name n     -> n
    Id_Reserved r -> renderReservedId r

renderReservedId :: ReservedId -> Text
renderReservedId r =
  case r of
    RId_SetVn -> "SETVN"
    RId_If    -> "IF"
    RId_Then  -> "THEN"
    RId_Goto  -> "GOTO"
    RId_While -> "WHILE"
    RId_Do    -> "DO"
    RId_End   -> "END"
