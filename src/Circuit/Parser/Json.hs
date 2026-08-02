-- | JSON for the circuits ecosystem: the parsing contact of aeson, rebuilt
-- on "Circuit.Parser" combinators.
--
-- Recursive descent over the @Uncons ByteString Char@ stream; string and
-- number payloads are captured as zero-copy slices ('bs') and converted
-- after recognition, so UTF-8 stays bytes until 'unescape' decodes it once.
--
-- Grammar notes: object and array tails commit on @,@ — a trailing comma
-- is a parse failure, not a short list. Numbers follow the JSON grammar
-- exactly (@-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?@), leading
-- zeros rejected.
--
-- Two paths, one tree: this module is the combinator path ('decodeJson' is
-- the boundary); "Circuit.Parser.Json.Lexer" is the fast path, a flat
-- zero-copy token stream for when composition is not the point.
module Circuit.Parser.Json
  ( -- * The tree
    Json (..),

    -- * Parsing
    value,
    jstring,
    jnumber,
    ws,

    -- * Conversion
    bsToScientific,

    -- * Boundary
    decodeJson,

    -- * Fast path
    JsonToken (..),
    runJsonLexerBS,

    -- * Pieces
    unescape,
  )
where

import Circuit.Parser
import Circuit.Parser.Json.Lexer (JsonToken (..), runJsonLexerBS)
import Circuit.Parser.Json.Unescape (unescape)
import Circuit.Parser.Json.Value (Json (..))
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word8)

-- $setup
-- >>> import Data.ByteString.Char8 qualified as C

-- | JSON whitespace: space, newline, carriage return, tab. Nothing else.
ws :: Parser Identity ByteString Char ()
ws = skipWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')

-- | Parse any JSON value, leading whitespace included.
value :: Parser Identity ByteString Char Json
value = ws *> atom
  where
    atom =
      jobject
        <|> jarray
        <|> (JString <$> jstring)
        <|> (string "true" $> JBool True)
        <|> (string "false" $> JBool False)
        <|> (string "null" $> JNull)
        <|> (JNumber <$> jnumber)

-- | Parse a JSON string literal (quotes included) to 'Text'.
--
-- Recognition is byte-level — the raw slice between the quotes is captured
-- with escapes intact and decoded once by 'unescape'.
jstring :: Parser Identity ByteString Char Text
jstring = do
  _ <- char '"'
  raw <- bs (skipMany (void (char '\\' *> anyToken) <|> void (satisfy isPlain)))
  _ <- char '"'
  either (const empty) pure (unescape raw)
  where
    isPlain c = c /= '"' && c /= '\\' && c >= ' '

-- | Parse a JSON number to 'Scientific'.
jnumber :: Parser Identity ByteString Char Scientific
jnumber = bsToScientific <$> bs numberRecog
  where
    numberRecog = do
      _ <- optional (char '-')
      _ <- void (char '0') <|> (satisfy (\c -> c >= '1' && c <= '9') *> skipWhile isDigit)
      _ <- optional (char '.' *> digits1)
      _ <- optional (void (char 'e' <|> char 'E') *> optional (void (char '+' <|> char '-')) *> digits1)
      pure ()
    digits1 = satisfy isDigit *> skipWhile isDigit

-- | Convert a recognised JSON number slice to 'Scientific'.
--
-- The slice is trusted to match the JSON number grammar (this is what
-- 'jnumber' recognises); anything else is garbage in, garbage out.
--
-- >>> bsToScientific (C.pack "123")
-- 123.0
--
-- >>> bsToScientific (C.pack "-0.5")
-- -0.5
--
-- >>> bsToScientific (C.pack "1e-3")
-- 1.0e-3
bsToScientific :: ByteString -> Scientific
bsToScientific s0 = scientific signedCoef (expN - B.length fracD)
  where
    (neg, s1) = case B.uncons s0 of
      Just (45, r) -> (True, r) -- '-'
      _ -> (False, s0)
    (intD, s2) = B.span isDigitW s1
    (fracD, s3) = case B.uncons s2 of
      Just (46, r) -> B.span isDigitW r -- '.'
      _ -> (B.empty, s2)
    expN = case B.uncons s3 of
      Just (_, r) -> signedInt r -- 'e' or 'E', then [+-]?digits
      Nothing -> 0
    coef = digitsToInteger intD * 10 ^ B.length fracD + digitsToInteger fracD
    signedCoef = if neg then negate coef else coef

-- | Parse a JSON document: a value, optional trailing whitespace, and
-- nothing else.
--
-- Failure is a flat @Left@ — the combinator runner backtracks to the
-- original stream, so there is no honest offset to report. For offsets,
-- tokenize first with "Circuit.Parser.Json.Lexer".
--
-- >>> decodeJson (C.pack "{\"a\": [1, true, null]}")
-- Right (JObject [("a",JArray [JNumber 1.0,JBool True,JNull])])
--
-- >>> decodeJson (C.pack "  \"a\\nb\"  ")
-- Right (JString "a\nb")
--
-- >>> decodeJson (C.pack "[1,]")
-- Left "invalid JSON"
--
-- >>> decodeJson (C.pack "01")
-- Left "invalid JSON"
decodeJson :: ByteString -> Either String Json
decodeJson s = case runParserIdentity (value <* ws <* endOfInput) s of
  This j -> Right j
  These j _ -> Right j
  That _ -> Left "invalid JSON"

-- * internals

jobject :: Parser Identity ByteString Char Json
jobject = do
  _ <- char '{'
  _ <- ws
  (char '}' $> JObject []) <|> obj
  where
    obj = do
      p <- pair
      _ <- ws
      JObject <$> tail' [p]
    pair = do
      k <- jstring
      _ <- ws
      _ <- char ':'
      v <- value
      pure (k, v)
    tail' acc = do
      c <- satisfy (\c -> c == ',' || c == '}')
      case c of
        ',' -> do
          _ <- ws
          p <- pair
          _ <- ws
          tail' (p : acc)
        _ -> pure (reverse acc)

jarray :: Parser Identity ByteString Char Json
jarray = do
  _ <- char '['
  _ <- ws
  (char ']' $> JArray V.empty) <|> arr
  where
    arr = do
      x <- value
      _ <- ws
      JArray . V.fromList <$> tail' [x]
    tail' acc = do
      c <- satisfy (\c -> c == ',' || c == ']')
      case c of
        ',' -> do
          x <- value
          _ <- ws
          tail' (x : acc)
        _ -> pure (reverse acc)

isDigitW :: Word8 -> Bool
isDigitW w = w >= 0x30 && w <= 0x39

digitsToInteger :: ByteString -> Integer
digitsToInteger = B.foldl' (\acc w -> acc * 10 + fromIntegral (w - 0x30)) 0

-- | @[+-]?digits@ to Int. Trusted input.
signedInt :: ByteString -> Int
signedInt s = case B.uncons s of
  Just (43, r) -> digitsToInt r -- '+'
  Just (45, r) -> negate (digitsToInt r) -- '-'
  _ -> digitsToInt s
  where
    digitsToInt = B.foldl' (\acc w -> acc * 10 + fromIntegral (w - 0x30)) 0
