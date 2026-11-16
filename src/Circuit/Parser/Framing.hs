{-# LANGUAGE OverloadedStrings #-}

-- | Concrete JSON and JSONL framing for text posts.
--
-- A stored row carries @id@, @ts@, @from@, @channel@, @to@, @thread@ and
-- @body@. Timestamps use second-precision UTC in @%Y-%m-%dT%H:%M:%S@ form.
-- An absent @channel@ decodes as 'Nothing'; unknown fields are ignored.
-- Senders are nonempty. The legacy discard audience @[""]@ decodes as @[]@,
-- and this module is the sole implementation of that convention: a row whose
-- audience is @[""]@ delivers nowhere, including to a subscriber itself named
-- @""@, which 'Circuit.Post.deliversTo' would otherwise match.
--
-- Logs are oldest first, and encoding preserves that order. 'decodeLog'
-- ignores blank rows and rejects the first malformed row rather than
-- dropping it, reporting the one-based row number, the zero-based character
-- offset of the row in the input, and a reason. A row whose JSON is
-- syntactically broken names the lexer's byte offset inside the row when the
-- lexer can supply one.
module Circuit.Parser.Framing
  ( Stored,
    RowError (..),
    encodeStored,
    decodeStored,
    encodeRow,
    decodeRow,
    encodeLog,
    decodeLog,
  )
where

import Circuit.Parser.Json (Json (..), decodeJson, encodeJson)
import Circuit.Parser.Json.Lexer qualified as Lexer
import Circuit.Post (Post (..), PostId)
import Circuit.Shared (Stamped (..))
import Data.ByteString qualified as B
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific, base10Exponent, coefficient)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Data.Vector qualified as V
import Prelude

-- | A text post with its UTC timestamp and absolute post id.
type Stored = Stamped (UTCTime, PostId) (Post Text Text)

-- | A malformed JSONL row.
data RowError = RowError
  { rowNumber :: Int,
    rowOffset :: Int,
    rowReason :: Text
  }
  deriving (Eq, Show)

-- | Encode a stamped post as a JSON object.
encodeStored :: Stored -> Json
encodeStored (Stamped (ts, i) (Post from' channel' to' thread' body')) =
  JObject
    [ ("id", number i),
      ("ts", JString (formatTimestamp ts)),
      ("from", JString from'),
      ("channel", maybe JNull JString channel'),
      ("to", JArray (V.fromList (JString <$> to'))),
      ("thread", JArray (V.fromList (number <$> thread'))),
      ("body", JString body')
    ]

-- | Decode a stamped post from a JSON object.
decodeStored :: Json -> Either Text Stored
decodeStored (JObject fields) =
  Stamped
    <$> ((,) <$> (required "ts" fields >>= timestamp) <*> (required "id" fields >>= postId))
    <*> ( Post
            <$> (required "from" fields >>= sender)
            <*> optionalChannel fields
            <*> (normaliseAudience <$> arrayField "to" (text "to") fields)
            <*> arrayField "thread" postId fields
            <*> (required "body" fields >>= text "body")
        )
decodeStored _ = Left "stored post: expected object"

-- | Encode one stamped JSONL row, without its line terminator.
encodeRow :: Stored -> Text
encodeRow = decodeUtf8 . encodeJson . encodeStored

-- | Decode one stamped JSONL row. A row that is not one stored post is
-- rejected with a reason; a JSON syntax failure carries the lexer's byte
-- offset inside the row when the lexer can name one.
decodeRow :: Text -> Either Text Stored
decodeRow line =
  case decodeJson (encodeUtf8 line) of
    Left err -> Left (syntaxError (encodeUtf8 line) err)
    Right value -> decodeStored value

syntaxError :: B.ByteString -> String -> Text
syntaxError bytes err =
  case Lexer.runJsonLexerBS bytes of
    Left (offset, reason) ->
      T.pack err <> " at byte " <> T.pack (show offset) <> ": " <> T.pack reason
    Right _ -> T.pack err

-- | Encode an oldest-first log as JSONL, including a final line terminator
-- for every row.
encodeLog :: [Stored] -> Text
encodeLog = T.unlines . fmap encodeRow

-- | Decode an oldest-first JSONL log. Every nonblank row must decode.
decodeLog :: Text -> Either RowError [Stored]
decodeLog content = catMaybes <$> traverse decodeAt rows
  where
    lines' = T.lines content
    offsets = scanl (\offset line -> offset + T.length line + 1) 0 lines'
    rows = zip3 [1 ..] offsets lines'
    decodeAt (row, offset, line)
      | T.null (T.strip line) = Right Nothing
      | otherwise =
          either
            (Left . RowError row offset)
            (Right . Just)
            (decodeRow line)

formatTimestamp :: UTCTime -> Text
formatTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

timestamp :: Json -> Either Text UTCTime
timestamp (JString value) =
  maybe
    (Left "ts: expected %Y-%m-%dT%H:%M:%S UTC timestamp")
    Right
    (parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack value))
timestamp _ = Left "ts: expected string"

required :: Text -> [(Text, Json)] -> Either Text Json
required key fields =
  case values key fields of
    [value] -> Right value
    [] -> Left (key <> ": missing")
    _ -> Left (key <> ": duplicate")

values :: Text -> [(Text, Json)] -> [Json]
values key fields = [value | (key', value) <- fields, key == key']

optionalChannel :: [(Text, Json)] -> Either Text (Maybe Text)
optionalChannel fields =
  case values "channel" fields of
    [] -> Right Nothing
    [JNull] -> Right Nothing
    [JString value] -> Right (Just value)
    [_] -> Left "channel: expected string or null"
    _ -> Left "channel: duplicate"

arrayField :: Text -> (Json -> Either Text a) -> [(Text, Json)] -> Either Text [a]
arrayField key decode fields =
  required key fields >>= \value ->
    case value of
      JArray items -> traverse decode (V.toList items)
      _ -> Left (key <> ": expected array")

text :: Text -> Json -> Either Text Text
text _ (JString value) = Right value
text key _ = Left (key <> ": expected string")

sender :: Json -> Either Text Text
sender (JString value)
  | T.null value = Left "from: expected nonempty string"
  | otherwise = Right value
sender _ = Left "from: expected string"

normaliseAudience :: [Text] -> [Text]
normaliseAudience [""] = []
normaliseAudience audience = audience

postId :: Json -> Either Text PostId
postId (JNumber value) =
  maybe (Left "id/thread: expected nonnegative integer") Right (naturalNumber value)
postId _ = Left "id/thread: expected number"

number :: PostId -> Json
number = JNumber . fromIntegral

naturalNumber :: Scientific -> Maybe PostId
naturalNumber value
  | c < 0 = Nothing
  | e >= 0 = Just (fromInteger (c * 10 ^ e))
  | r == 0 = Just (fromInteger q)
  | otherwise = Nothing
  where
    c = coefficient value
    e = base10Exponent value
    (q, r) = c `divMod` (10 ^ negate e)
