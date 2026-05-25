-- | Numeric primitives and string conversions for "Circuit.Parser".
module Circuit.Parser.Primitives
  ( -- * Character predicates
    isDigit,
    isLatinLetter,

    -- * Numeric parsers
    digit,
    digits,
    int,
    double,
    signed,

    -- * String / ByteString conversions
    strToUtf8,
    utf8ToStr,
  )
where

import Circuit.Parser
import Data.ByteString (ByteString)
import Data.Char (isAsciiLower, isAsciiUpper, ord)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.Encoding.Error

-- | Is the character an ASCII digit?
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- | Is the character a Latin letter?
isLatinLetter :: Char -> Bool
isLatinLetter c = isAsciiLower c || isAsciiUpper c

-- | Parse a single digit.
digit :: Parser ByteString Char Int
digit = (\c -> ord c - ord '0') <$> satisfy isDigit

-- | Parse one or more digits, returning @(place, value)@ where @place@ is
-- @10 ^ number_of_digits@.
--
-- >>> import Circuit.Parser (runParser)
-- >>> runParser digits "123"
-- These (1000,123) ""
digits :: Parser ByteString Char (Int, Int)
digits = do
  ds <- some digit
  let place = 10 ^ length ds
      n = foldl (\acc d -> acc * 10 + d) 0 ds
  return (place, n)

-- | Parse a non-empty sequence of digits as an integer.
int :: Parser ByteString Char Int
int = do
  (place, n) <- digits
  if place == 1 then empty else return n

-- | Parse a floating-point number.
double :: Parser ByteString Char Double
double = do
  (placel, nl) <- digits
  mfrac <- optional (char '.' *> digits)
  case mfrac of
    Nothing ->
      if placel == 1 then empty else return (fromIntegral nl)
    Just (placer, nr) ->
      return (fromIntegral nl + fromIntegral nr / fromIntegral placer)

-- | Optionally negate the result of a parser.
signed :: (Num a) => Parser ByteString Char a -> Parser ByteString Char a
signed p = do
  m <- optional (char '-')
  case m of
    Nothing -> p
    Just _ -> negate <$> p

-- | Encode a 'String' as UTF-8 'ByteString'.
strToUtf8 :: String -> ByteString
strToUtf8 = encodeUtf8 . T.pack

-- | Decode a UTF-8 'ByteString' to 'String'.
utf8ToStr :: ByteString -> String
utf8ToStr = T.unpack . decodeUtf8With lenientDecode
