{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Data.GCode.Lexical.Lexer where

import           Data.GCode.Lexical.Position (Positioned (Positioned),
                                              Position (Position),
                                              fromSourcePos, decCol)
import           Data.GCode.Lexical.Tokens   (Token (..))

import           Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Mega (Token)
import           Text.Megaparsec (ErrorComponent, ShowErrorComponent,
                                  char', space, representFail,
                                  representIndentation, showErrorComponent,
                                  label, getPosition, choice)
import           Text.Megaparsec.Prim (MonadParsec)


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
  [ lexConst Tok_LBracket $ symbol "["
  , lexConst Tok_RBracket $ symbol "]"
  ]
  
-------------------------------------------------------------------------------

symbol :: Lexer s m => String -> m (Positioned ())
symbol = lexUnit . positioned . stringws'

-------------------------------------------------------------------------------

lexAs :: Lexer s m => (a -> b) -> m (Positioned a) -> m (Positioned b)
lexAs f mp = fmap f <$> mp

lexConst :: Lexer s m => b -> m (Positioned a) -> m (Positioned b)
lexConst b = lexAs (const b)

lexUnit :: Lexer s m => m (Positioned a) -> m (Positioned ())
lexUnit = lexConst ()

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
        [] -> error "should not be called on an empty string"
        [x] ->
          do
            _ <- char' x
            pure [x]
        s@(x:xs) ->
          do
            _ <- char' x
            space
            _ <- stringws' xs
            pure s
