{-# LANGUAGE OverloadedStrings #-}

module Data.GCode.Lexical.Tokens where

import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import qualified Data.Text       as T (concat, pack)

data Token
  =
    --
    -- Delimiters
    --
    Tok_LBracket                -- ^ [
  | Tok_RBracket                -- ^ ]

    --
    -- Names (alphanumeric)
    --
  | Tok_Name !Name

    --
    -- Symbolic Operators
    --
  | Tok_Op !SymbolOperator      -- ^ plus, minus, multiply, divide, hash, etc.

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

data Name
  = Name_Id !Text
  | Name_Reserved !ReservedId
  deriving (Eq, Show)

data ReservedId
  = RId_SetVn
  | RId_If
  | RId_Then
  | RId_Goto
  | RId_While
  | RId_Do
  | RId_End
  | RId_Or
  | RId_And
  | RId_Xor
  deriving (Eq, Show)

data SymbolOperator
  = Op_Hash
  | Op_Add
  | Op_Sub
  | Op_Mul
  | Op_Div
  | Op_Pow
  deriving (Eq, Show)

renderToken :: Token -> Text
renderToken t =
  case t of
    Tok_LBracket       -> "["
    Tok_RBracket       -> "]"
    Tok_Name x         -> renderName x
    Tok_Op s           -> renderSymbolOperator s
    Tok_LitNumber n    -> T.pack . show $ n
    Tok_ParenComment c -> T.concat ["(", c, ")"]
    Tok_SemiComment c  -> T.concat [";", c]

renderName :: Name -> Text
renderName x =
  case x of
    Name_Id n       -> n
    Name_Reserved r -> renderReservedId r

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
    RId_Or    -> "OR"
    RId_And   -> "AND"
    RId_Xor   -> "XOR"

renderSymbolOperator :: SymbolOperator -> Text
renderSymbolOperator s =
  case s of
    Op_Hash -> "#"
    Op_Add  -> "+"
    Op_Sub  -> "-"
    Op_Mul  -> "*"
    Op_Div  -> "/"
    Op_Pow  -> "**"
