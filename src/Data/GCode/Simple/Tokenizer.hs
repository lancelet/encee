{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}

{-|
Module      : Data.GCode.Simple.Tokenizer
Description : Tokenizes GCode streams
Copyright   : (c) Jonathan Merritt, 2016
License     : BSD3
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental
Portability : POSIX

The tokenizer is the first stage in reading GCode. Its function is to produce
tokens from a 'Source' efficiently. The source is scanned using fast tests that
assess whether a character is part of an allowed set (or "character class") for
each token.

== Usage

To produce a list of tokens:

>>> :set -XOverloadedStrings
>>> import qualified Data.ByteString.Lazy.Char8 as BS
>>> let gcode = "G01 X0 Y0 Z0 ; some GCode" :: BS.ByteString
>>> let tokens = tokenize (byteStringSource gcode)
>>> length tokens
5 

== Whitespace in GCode Tokens

The tokenization of GCode is complicated slightly by handling whitespace:

- in the original GCode specification
  (<http://www.nist.gov/manuscript-publication-search.cfm?pub_id=823374 Kramer et al. 2000>):
  "Spaces and tabs are allowed anywhere on a line of code and do
  not change the meaning of the line, except inside comments",

- spaces are included in a line of GCode for the purposes of
  <http://reprap.org/wiki/G-code#.2A:_Checksum RepRap checksumming>.

Therefore, if we are to tokenize before checksumming, it is important to retain
a representation of the whitespace inside the tokens. This is done by ensuring
that the original character sequence can be reconstructed verbatim in each
token. The trade-off is that we don't fully parse within each token at this
stage. For example, the 'TokField' token should contain a number, but we don't
yet fully parse that number.

To illustrate the whitespace retention, consider the following GCode program:

>>> :set -XOverloadedStrings
>>> let gcode = "G 01 "
>>> tokenDetails <$> (head . tokenize . byteStringSource) gcode
Right (TokField {tokFieldChar = 'G', tokFieldNum = " 01 "})

So, the numbers associated with each word are stored in their original character
format so that no information is lost.

-}
module Data.GCode.Simple.Tokenizer
  ( -- * Tokenizer
    tokenize
    -- * Source of bytestrings
  , ByteStringSource
  , byteStringSource
    -- * Types
  , Token
    ( Token
    , tokenPos
    , tokenDetails
    )
  , TokenDetails
    ( UnassignedWhitespace
    , TokField
    , tokFieldChar
    , tokFieldNum
    , SemicolonComment
    , ParensComment
    , Newlines
    , Percent
    , BlockDelete
    )
  , Pos
    ( Pos
    , posLine
    , posColumn
    )
  , TokenizerError
    ( TokenizerError
    , tokenizerErrorType
    , tokenizerErrorPos
    )
  , TokenizerErrorType
    ( NoClosingParen
    , MissingRawFieldNumber
    , UnknownChar
    )
    -- * Type classes
  , Source
    ( srcNull
    , srcPos
    , srcHead
    , srcTail
    , srcSpan
    , srcTakeTillAfter
    )
  , ASCIIBytes
    ( asciiBytesNull
    , asciiBytesTail
    , asciiBytesInit
    )
  ) where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS (break, elemIndex, head, init,
                                                   length, null, span, splitAt,
                                                   tail)
import qualified Data.Char                  as DC (isAsciiLower, isAsciiUpper,
                                                   isDigit)
import           Data.Word                  (Word32)


-------------------------------------------------------------------------------
-- Token position
-------------------------------------------------------------------------------

-- | Position of a token within its original stream.
data Pos = Pos
  { -- | Line number.
    posLine   :: {-# UNPACK #-} !Word32
    -- | Column number.
  , posColumn :: {-# UNPACK #-} !Word32
  } deriving (Eq, Show)


-- | Updates position based on an incoming character.
updatePosChar :: Pos -> Char -> Pos
updatePosChar (Pos l c) x
  | x == '\n' = Pos (l+1) 0
  | otherwise = Pos l (c+1)


-- | Updates position based on a 'ByteString'.
updatePosBS :: Pos -> ByteString -> Pos
updatePosBS (Pos l c) bs =
  let
    (_, rb) = BS.break (== '\n') bs
  in
    if BS.null rb
      then Pos l (c + fromIntegral (BS.length bs))
      else updatePosBS (Pos (l + 1) 0) (BS.tail rb)


-------------------------------------------------------------------------------
-- Token contents
-------------------------------------------------------------------------------

-- | ASCIIBytes is a sequence of 8-bit ASCII characters.
class ASCIIBytes a where
  -- | Checks if the sequence is empty.
  asciiBytesNull :: a -> Bool
  -- | Returns all but the first character.
  asciiBytesTail :: a -> a
  -- | Returns all but the last character.
  asciiBytesInit :: a -> a


instance ASCIIBytes ByteString where
  asciiBytesNull = BS.null
  asciiBytesTail = BS.tail
  asciiBytesInit = BS.init


-------------------------------------------------------------------------------
-- Token
-------------------------------------------------------------------------------

-- | GCode token.
data Token a = Token
  { -- | Position of the GCode token within its original stream of ASCII
    --   characters.
    tokenPos  :: {-# UNPACK #-} !Pos
    -- | Details of the token; its type and contents.
  , tokenDetails :: TokenDetails a
  }

deriving instance Eq a => Eq (Token a)
deriving instance Show a => Show (Token a)


-- | Details of a token (its type and contents).
data TokenDetails a

  -- | Unassigned whitespace.
  --
  --   Whitespace is usually included as part of a regular token. However,
  --   unassigned whitespace can occur at various other locations such as:
  --    - the beginning of lines,
  --    - after the closing parenthesis of a comment
  = UnassignedWhitespace !a

  -- | Field.
  --
  --   Each field consist of a character and a number.
  | TokField
    { -- | Character associated with a field (also called a "word" in GCode).
      tokFieldChar :: {-# UNPACK #-} !Char
      -- | Raw numeric value associated with the field.
    , tokFieldNum  :: !a
    }

  -- | Semicolon comment.
  --
  --   Semicolon comments start with a semicolon and continue to the first
  --   newline. The contents of the token do not include the starting
  --   semicolon.
  | SemicolonComment !a

  -- | Comment delimited by parentheses.
  --
  --   The contents of the token do not include the parentheses.
  | ParensComment !a

  -- | Newlines.
  | Newlines !a

  -- | Percent symbol.
  | Percent

  -- | Block delete (slash) symbol.
  | BlockDelete


deriving instance Eq a => Eq (TokenDetails a)
deriving instance Show a => Show (TokenDetails a)


-------------------------------------------------------------------------------
-- Tokenizing source
-------------------------------------------------------------------------------

-- | A source of tokens.
--
--   Type 's' is the source, type 'r' is the raw type of token contents
--   (usually an instance of 'TokenContentsRaw').
--
--   Sources provide O(1) byte-based operations for quickly scanning tokens.
class Source s r | s -> r where

  -- | Is the source is empty?
  srcNull :: s -> Bool

  -- | Current position of the source.
  srcPos :: s -> Pos

  -- | First character of the source.
  --
  --   The source must be non-null otherwise an exception will be thrown.
  srcHead :: s -> Char

  -- | Remainder of the source after removing the first character.
  --
  --   The source must be non-null otherwise an exception will be thrown.
  srcTail :: s -> s

  -- | Splits a source according to a predicate.
  --
  --   All characters in the first element of the returned tuple are contiguous
  --   values that satisfy the given predicate. The second element contains the
  --   rest of the source.
  srcSpan :: s -> (Char -> Bool) -> (r, s)

  -- | Takes characters from a source until after a given character is reached.
  srcTakeTillAfter :: s -> Char -> Maybe (r, s)


-- | Source built on lazy 'ByteString's.
data ByteStringSource = ByteStringSource
  { bsByteString :: !ByteString
  , bsPos        :: {-# UNPACK #-} !Pos
  }


-- | Creates a 'ByteStringSource' from a 'ByteString'.
byteStringSource :: ByteString -> ByteStringSource
byteStringSource bs = ByteStringSource bs (Pos 0 0)


instance Source ByteStringSource ByteString where

  srcNull = BS.null . bsByteString

  srcPos = bsPos

  srcHead = BS.head . bsByteString

  srcTail (ByteStringSource bs pos) = ByteStringSource bs' pos'
    where
      h = BS.head bs
      bs' = BS.tail bs
      pos' = updatePosChar pos h

  srcSpan (ByteStringSource bs pos) p = (l, ByteStringSource r pos')
    where
      (l, r) = BS.span p bs
      pos' = updatePosBS pos l

  srcTakeTillAfter (ByteStringSource bs pos) w =
    case BS.elemIndex w bs of
      Nothing -> Nothing
      Just i ->
        let
          (l, r) = BS.splitAt (i+1) bs
          pos' = updatePosBS pos l
        in
          Just (l, ByteStringSource r pos')


-- | Utility function to scan based on a character class.
--
--   Scans a source for the initial contiguous characters that match the
--   provided character predicate (the "character class"). Then, based on the
--   original position of the source and the scanned result, construct a new
--   value using the provided constructor.
scanClass :: Source a r
          => (Char -> Bool)   -- ^ character class predicat
          -> (Pos -> r -> b)  -- ^ constructs a result
          -> a                -- ^ source
          -> (b, a)           -- ^ result and remaining source
scanClass predicate constructor s = (constructor pos l, r)
  where
    pos = srcPos s
    (l, r) = srcSpan s predicate


-------------------------------------------------------------------------------
-- Tokenizer Errors
-------------------------------------------------------------------------------

-- | Classifies types of error that can occur while tokenizing.
data TokenizerErrorType
  -- | A comment had a starting parenthesis, but no closing parenthesis.
  = NoClosingParen
  -- | A field had no characters that could possibly be a field number.
  | MissingRawFieldNumber
  -- | An unknown character was encountered.
  | UnknownChar
  deriving (Eq, Show)


-- | Tokenizer errors have a type and position.
data TokenizerError = TokenizerError
  { tokenizerErrorType :: !TokenizerErrorType
  , tokenizerErrorPos  :: {-# UNPACK #-} !Pos
  } deriving (Eq, Show)


-------------------------------------------------------------------------------
-- Scanning tokens from a source
-------------------------------------------------------------------------------

-- | Tokenizes an entire 'Source'.
--
--   This function scans the provided 'Source' and produces a list where each
--   element is either a 'Token' or a 'TokenizerError'. If an error is
--   detected, only the last element of the returned list will contain a
--   error (ie. tokenization stops after the first error is detected).
tokenize :: (Source a r, ASCIIBytes r)
         => a
         -- ^ source to tokenize
         -> [Either TokenizerError (Token r)]
         -- ^ list of produced tokens
tokenize s =
  if srcNull s
    then []
    else
      case nextToken s of
        Left err -> [Left err]
        Right (token, remainder) ->
          (Right token) : tokenize remainder


-- | Extracts just one token from a source.
nextToken :: (Source a r, ASCIIBytes r)
          => a
          -> Either TokenizerError (Token r, a)
nextToken s = f s
  where
    h = srcHead s
    f = case () of _
                     | isWhitespace h -> scanWhitespace
                     | isFieldChar h  -> scanField
                     | h == ';'       -> scanSemicolonComment
                     | h == '('       -> scanParensComment
                     | isNewline h    -> scanNewlines
                     | h == '%'       -> scanPercent
                     | h == '/'       -> scanBlockDelete
                     | otherwise      -> errorUnknownChar


mkaToken :: (a -> TokenDetails a) -> Pos -> a -> Token a
mkaToken constructor pos x = Token pos (constructor x)


scanWhitespace :: Source a r => a -> Either TokenizerError (Token r, a)
scanWhitespace = Right . scanClass isWhitespace (mkaToken UnassignedWhitespace)


scanField :: (Source a r, ASCIIBytes r)
          => a -> Either TokenizerError (Token r, a)
scanField s =
  let
    pos = srcPos s
    w = srcHead s
    (l, r) = srcSpan (srcTail s) isTokFieldNum
  in
    if asciiBytesNull l
      then Left $ TokenizerError MissingRawFieldNumber pos
      else Right (Token pos (TokField w l), r)


scanSemicolonComment :: (Source a r, ASCIIBytes r)
                     => a -> Either TokenizerError (Token r, a)
scanSemicolonComment = Right . scanClass (not . isNewline) f
  where
    f pos x = Token pos (SemicolonComment (asciiBytesTail x))


scanParensComment :: (Source a r, ASCIIBytes r)
                  => a -> Either TokenizerError (Token r, a)
scanParensComment s =
  let
    pos = srcPos s
  in
    case srcTakeTillAfter s ')' of
      Nothing -> Left $ TokenizerError NoClosingParen pos
      Just (l, r) ->
        let
          x = ParensComment $ (asciiBytesInit . asciiBytesTail) l
        in
          Right (Token pos x, r)


scanNewlines :: Source a r => a -> Either TokenizerError (Token r, a)
scanNewlines = Right . scanClass isNewline (mkaToken Newlines)


scanPercent :: Source a r => a -> Either TokenizerError (Token r, a)
scanPercent s = Right (Token (srcPos s) Percent, srcTail s)


scanBlockDelete :: Source a r => a -> Either TokenizerError (Token r, a)
scanBlockDelete s = Right (Token (srcPos s) BlockDelete, srcTail s)


errorUnknownChar :: Source a r => a -> Either TokenizerError (Token r, a)
errorUnknownChar s = Left $ TokenizerError UnknownChar (srcPos s)


-------------------------------------------------------------------------------
-- Base character classes
-------------------------------------------------------------------------------

isDigit :: Char -> Bool
isDigit = DC.isDigit


isNewline :: Char -> Bool
isNewline c
   = c == '\n'
  || c == '\r'


isWhitespace :: Char -> Bool
isWhitespace c
   = c == ' '
  || c == '\t'


isLetter :: Char -> Bool
isLetter c
   = DC.isAsciiUpper c
  || DC.isAsciiLower c


isFieldChar :: Char -> Bool
isFieldChar c
   = isLetter c
  || c == '*'


isNumber :: Char -> Bool
isNumber c
   = isDigit c
  || c == '.'
  || c == '+'
  || c == '-'


isTokFieldNum :: Char -> Bool
isTokFieldNum c
   = isNumber c
  || isWhitespace c
