{-# OPTIONS_GHC -Wwarn -Wno-unused-imports #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.GCode.Lexical.Lexer where

import           Data.GCode.Lexical.Position (Position (Position),
                                              Positioned (Positioned), decCol,
                                              fromSourcePos)
import           Data.GCode.Lexical.Tokens   (Token (..))

import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map (fromList)
import           Text.Megaparsec             ((<|>))
import           Text.Megaparsec             (ErrorComponent,
                                              ShowErrorComponent, char', choice,
                                              getPosition, label, oneOf,
                                              representFail,
                                              representIndentation,
                                              showErrorComponent, space)
import qualified Text.Megaparsec             as Mega (Token)
import           Text.Megaparsec.Prim        (MonadParsec)


type Lexer s m = (MonadParsec LexerError s m, Mega.Token s ~ Char)

data LexerError
  = LexerUnknownError
    deriving (Eq, Ord)

instance ErrorComponent LexerError where
  representFail _ = LexerUnknownError
  representIndentation _ _ _ = LexerUnknownError

instance ShowErrorComponent LexerError where
  showErrorComponent _ = ""

-------------------------------------------------------------------------------

lexDelimiter :: Lexer s m => m (Positioned Token)
lexDelimiter =
  choice
  [ lexConst Tok_LBracket $ symbolChar '['
  , lexConst Tok_RBracket $ symbolChar ']'
  ]

lexOperator :: Lexer s m => m (Positioned Token)
lexOperator =
  choice
  [ lexConst Tok_Hash  $ symbolChar '#'
  , lexConst Tok_Pow   $ symbol     "**"
  , lexConst Tok_Mul   $ symbolChar '*'
  , lexConst Tok_Div   $ symbolChar '/'
  , lexConst Tok_Mod   $ symbol     "MOD"
  , lexConst Tok_Plus  $ symbolChar '+'
  , lexConst Tok_Minus $ symbolChar '-'
  , lexConst Tok_Eq    $ symbol     "EQ"
  , lexConst Tok_Ne    $ symbol     "NE"
  , lexConst Tok_Gt    $ symbol     "GT"
  , lexConst Tok_Ge    $ symbol     "GE"
  , lexConst Tok_Lt    $ symbol     "LT"
  , lexConst Tok_Le    $ symbol     "LE"
  , lexConst Tok_And   $ symbol     "AND"
  , lexConst Tok_Or    $ symbol     "OR"
  , lexConst Tok_Xor   $ symbol     "XOR"
  ]

lexControlFlow :: Lexer s m => m (Positioned Token)
lexControlFlow =
  choice
  [ lexConst Tok_If        $ symbol "IF"
  , lexConst Tok_ElseIf    $ symbol "ELSEIF"
  , lexConst Tok_Else      $ symbol "ELSE"
  , lexConst Tok_EndIf     $ symbol "ENDIF"
  , lexConst Tok_Repeat    $ symbol "REPEAT"
  , lexConst Tok_EndRepeat $ symbol "ENDREPEAT"
  , lexConst Tok_Do        $ symbol "DO"
  , lexConst Tok_While     $ symbol "WHILE"
  , lexConst Tok_EndWhile  $ symbol "ENDWHILE"
  , lexConst Tok_Sub       $ symbol "SUB"
  , lexConst Tok_EndSub    $ symbol "ENDSUB"
  , lexConst Tok_Call      $ symbol "CALL"
  , lexConst Tok_Return    $ symbol "RETURN"
  ]

-------------------------------------------------------------------------------

symbol :: Lexer s m => String -> m (Positioned ())
symbol = lexConst () . positioned . stringws'

symbolChar :: Lexer s m => Char -> m (Positioned ())
symbolChar c = symbol [c]

-------------------------------------------------------------------------------

lexAs :: Lexer s m => (a -> b) -> m (Positioned a) -> m (Positioned b)
lexAs = fmap . fmap

lexConst :: Lexer s m => b -> m (Positioned a) -> m (Positioned b)
lexConst = lexAs . const

-------------------------------------------------------------------------------

positioned :: Lexer s m => m a -> m (Positioned a)
positioned lexer =
  do
    start <- position
    x <- lexer
    end <- decCol <$> position
    pure $ Positioned start end x


position :: Lexer s m => m Position
position = fromSourcePos <$> getPosition

-------------------------------------------------------------------------------

-- | Parses a case-insensitive string, allowing internal whitespace.
stringws' :: Lexer s m => String -> m String
stringws' str = label str parser
  where
    parser =
      case str of

        -- error for an empty string
        [] -> error "should not be called on an empty string"

        -- in the case of just one remaining character, we take *just* that
        --   character and no more
        [x] ->
          do
            _ <- char' x
            pure [x]

        -- in the case of more than one remaining character, we take the
        --   next character, then any whitespace, and recursively continue
        s@(x:xs) ->
          do
            _ <- char' x
            space
            _ <- stringws' xs
            pure s
