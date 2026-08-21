-- | CSV for the circuits ecosystem: RFC 4180 on "Circuit.Parser" combinators.
--
-- Where "Circuit.Parser.Json" skips whitespace, csv skips nothing — spaces
-- are data, line endings are structure. The dialect:
--
-- - line endings lenient: @\\r\\n@ | @\\n@ | @\\r@.
-- - quoting strict: @"@ opens a quoted field only at field start; inside a
--   quoted field @""@ is an escaped quote; after the closing quote only @,@,
--   EOL, or EOF may follow. A bare @"@ inside a plain field is rejected.
-- - a trailing comma is a field: @a,b,@ is three fields. An empty line is
--   one record of one empty field. The final record may omit its line
--   ending. Empty input is zero records.
-- - field-count consistency across records is not checked — consumer
--   policy, like duplicate keys in json.
module Circuit.Parser.Csv
  ( -- * The table
    Csv (..),

    -- * Parsing
    csv,
    record,
    field,

    -- * Boundary
    decodeCsv,
  )
where

import Circuit.Parser
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Vector (Vector)
import Data.Vector qualified as V

-- $setup
-- >>> import Data.ByteString.Char8 qualified as C

-- | A CSV table: rows of fields, source order. No header policy — headers
-- are a consumer convention, not a parsing fact.
newtype Csv = Csv (Vector (Vector Text))
  deriving (Eq, Show)

-- | Parse a whole CSV table.
csv :: Parser Identity ByteString Char Csv
csv = (endOfInput $> Csv V.empty) <|> rows
  where
    rows = do
      r <- record
      Csv . V.fromList . map V.fromList <$> loop [r]
    loop acc =
      ( do
          _ <- eol
          atEnd <- (endOfInput $> True) <|> pure False
          if atEnd
            then pure (reverse acc)
            else do
              r <- record
              loop (r : acc)
      )
        <|> pure (reverse acc)

-- | Parse one record: fields separated by commas. A trailing comma is a
-- trailing empty field.
record :: Parser Identity ByteString Char [Text]
record = do
  f <- field
  loop [f]
  where
    loop acc =
      ( do
          _ <- char ','
          f <- field
          loop (f : acc)
      )
        <|> pure (reverse acc)

-- | Parse one field, quoted or plain. 'try' is load-bearing: without it, a
-- failed quoted attempt (say, an unterminated quote) would hand the plain
-- alternative the stream at the point of failure — consumed quote and all —
-- and an unterminated quote would silently parse as an empty field.
field :: Parser Identity ByteString Char Text
field = try quoted <|> plain
  where
    plain = do
      raw <- bs (skipWhile (\c -> c /= ',' && c /= '\r' && c /= '\n' && c /= '"'))
      either (const empty) pure (decodeUtf8' raw)
    quoted = do
      _ <- char '"'
      raw <- bs (skipMany (void (string "\"\"") <|> void (satisfy (/= '"'))))
      _ <- char '"'
      either (const empty) pure (unquote raw)

-- | Decode a quoted field's raw slice: @""@ collapses to @"@, then UTF-8.
unquote :: ByteString -> Either String Text
unquote raw = case decodeUtf8' raw of
  Left e -> Left (show e)
  Right t -> Right (T.intercalate (T.pack "\"") (T.splitOn (T.pack "\"\"") t))

-- | Line ending: CRLF, LF, or lone CR.
eol :: Parser Identity ByteString Char ()
eol = void (string "\r\n") <|> void (char '\n') <|> void (char '\r')

-- | Parse a CSV document, rejecting trailing garbage.
--
-- >>> decodeCsv (C.pack "a,b\r\nc,d\r\n")
-- Right (Csv [["a","b"],["c","d"]])
--
-- >>> decodeCsv (C.pack "name,note\n\"say \"\"hi\"\"\",ok")
-- Right (Csv [["name","note"],["say \"hi\"","ok"]])
--
-- >>> decodeCsv (C.pack "a,b,\n")
-- Right (Csv [["a","b",""]])
--
-- >>> decodeCsv (C.pack "")
-- Right (Csv [])
--
-- >>> decodeCsv (C.pack "a\"b,c")
-- Left "invalid CSV"
decodeCsv :: ByteString -> Either String Csv
decodeCsv s = case runParserIdentity (csv <* endOfInput) s of
  This c -> Right c
  These c _ -> Right c
  That _ -> Left "invalid CSV"
