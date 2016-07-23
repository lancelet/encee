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
                                              getPosition, label, representFail,
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
  [ lexConst (symbolChar '[') Tok_LBracket
  , lexConst (symbolChar ']') Tok_RBracket
  ]

{-
lexReservedIdentifier :: Lexer s m => m (Positioned Token)
lexReservedIdentifier = undefined
  where
    reservedIdentifiers =
      Map.fromList
      [ ("setvn", Tok_SetVn)
      , ("if"   , Tok_If   )
      , ("then" , Tok_Then )
      , ("goto" , Tok_Goto )
      , ("while", Tok_While)
      , ("do"   , Tok_Do   )
      , ("end"  , Tok_End  )
      ]
-}

-------------------------------------------------------------------------------

symbol :: Lexer s m => String -> m (Positioned ())
symbol s = lexConst ((positioned . stringws') s) ()

symbolChar :: Lexer s m => Char -> m (Positioned ())
symbolChar c = symbol [c]

-------------------------------------------------------------------------------

lexAs :: Lexer s m => m (Positioned a) -> (a -> b) -> m (Positioned b)
lexAs = flip (fmap . fmap)

lexConst :: Lexer s m => m (Positioned a) -> b -> m (Positioned b)
lexConst lexer c = lexAs lexer (const c)

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
