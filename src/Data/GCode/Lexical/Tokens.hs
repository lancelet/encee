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
    -- Operators
    --
  | Tok_Hash                    -- ^ #
  | Tok_Pow                     -- ^ **
  | Tok_Mul                     -- ^ *
  | Tok_Div                     -- ^ /
  | Tok_Mod                     -- ^ MOD
  | Tok_Plus                    -- ^ +
  | Tok_Minus                   -- ^ -
  | Tok_Eq                      -- ^ EQ
  | Tok_Ne                      -- ^ NE
  | Tok_Gt                      -- ^ GT
  | Tok_Ge                      -- ^ GE
  | Tok_Lt                      -- ^ LT
  | Tok_Le                      -- ^ LE
  | Tok_And                     -- ^ AND
  | Tok_Or                      -- ^ OR
  | Tok_Xor                     -- ^ XOR

    --
    -- Control Flow
    --
  | Tok_If                      -- ^ IF
  | Tok_ElseIf                  -- ^ ELSEIF
  | Tok_Else                    -- ^ ELSE
  | Tok_EndIf                   -- ^ ENDIF
  | Tok_Repeat                  -- ^ REPEAT
  | Tok_EndRepeat               -- ^ ENDREPEAT
  | Tok_Do                      -- ^ DO
  | Tok_While                   -- ^ WHILE
  | Tok_EndWhile                -- ^ ENDWHILE
  | Tok_Sub                     -- ^ SUB
  | Tok_EndSub                  -- ^ ENDSUB
  | Tok_Call                    -- ^ CALL
  | Tok_Return                  -- ^ RETURN

    --
    -- Literals
    --
  | Tok_WordChar !Char          -- ^ word character
  | Tok_Number !Scientific      -- ^ literal number
  | Tok_NamedParam !Text        -- ^ named parameter, delim by angle brackets

    --
    -- Comments
    --
  | Tok_ParenComment !Text      -- ^ parenthesis-delimited comment
  | Tok_SemiComment !Text       -- ^ semicolon-EOL-delimited comment

  deriving (Eq, Show)


renderToken :: Token -> Text
renderToken t =
  case t of
    --
    Tok_LBracket  -> "["
    Tok_RBracket  -> "]"
    --
    Tok_Hash      -> "#"
    Tok_Pow       -> "**"
    Tok_Mul       -> "*"
    Tok_Div       -> "/"
    Tok_Mod       -> "MOD"
    Tok_Plus      -> "+"
    Tok_Minus     -> "-"
    Tok_Eq        -> "EQ"
    Tok_Ne        -> "NE"
    Tok_Gt        -> "GT"
    Tok_Ge        -> "GE"
    Tok_Lt        -> "LT"
    Tok_Le        -> "LE"
    Tok_And       -> "AND"
    Tok_Or        -> "OR"
    Tok_Xor       -> "XOR"
    ---
    Tok_If        -> "IF"
    Tok_ElseIf    -> "ELSEIF"
    Tok_Else      -> "ELSE"
    Tok_EndIf     -> "ENDIF"
    Tok_Repeat    -> "REPEAT"
    Tok_EndRepeat -> "ENDREPEAT"
    Tok_Do        -> "DO"
    Tok_While     -> "WHILE"
    Tok_EndWhile  -> "ENDWHILE"
    Tok_Sub       -> "SUB"
    Tok_EndSub    -> "ENDSUB"
    Tok_Call      -> "CALL"
    Tok_Return    -> "RETURN"
    ---
    Tok_WordChar c     -> T.singleton c
    Tok_Number n       -> T.pack . show $ n
    Tok_NamedParam x   -> T.concat ["<", x, ">"]
    Tok_ParenComment x -> T.concat ["(", x, ")"]
    Tok_SemiComment x  -> T.concat [";", x]
    


{-
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

-}
