{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor      #-}

module Data.GCode.Lexical.Position where

import qualified Data.Char as DC ()
import           Data.Word (Word32)

import           Text.Megaparsec.Pos (SourcePos(SourcePos), unPos,
                                     unsafePos)


data Positioned a =
  Positioned
  { posStart :: !Position
  , posEnd   :: !Position
  , posTail  :: !a
  } deriving (Eq, Ord, Functor)


data Position =
  Position
  { posFile   :: !FilePath
  , posLine   :: !Word32
  , posColumn :: !Word32
  } deriving (Eq, Ord, Show)


deriving instance Show a => Show (Positioned a)


updatePosition :: Position -> Char -> Position
updatePosition (Position file line _  ) '\n' = Position file (line+1) 0
updatePosition (Position file line col) _    = Position file line (col+1)


toSourcePos :: Position -> SourcePos
toSourcePos (Position file line col) = SourcePos file line' col'
  where
    line' = (unsafePos . fromIntegral) line
    col'  = (unsafePos . fromIntegral) col


fromSourcePos :: SourcePos -> Position
fromSourcePos (SourcePos file line col) = Position file line' col'
  where
    line' = (fromIntegral . unPos) line
    col'  = (fromIntegral . unPos) col


decCol :: Position -> Position
decCol (Position file line col) = Position file line (col-1)
