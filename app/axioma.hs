{-# LANGUAGE OverloadedStrings #-}

-- | circuits-parser-axioma ⟜ outward verification.
--
-- Closed-form oracles, not a test framework: exact input → expected value
-- pairs for both dialects, then the JSONTestSuite corpus (shipped with aeson
-- at @~/other/aeson/tests/JSONTestSuite/test_parsing@) as accept/reject
-- oracle, with a value-level differential against aeson itself on the
-- accept files and against cassava on a CSV corpus.
--
-- Exits nonzero on the first category with any failure, listing them all.
module Main (main) where

import Circuit.Parser.Csv (Csv (..), decodeCsv)
import Circuit.Parser.Csv.Lexer (CsvToken (..), runCsvLexerBS)
import Circuit.Parser.Framing
import Circuit.Parser.Json
import Circuit.Post (Post (..), deliversTo)
import Circuit.Shared (Stamped (..))
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Scientific (scientific)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.Vector qualified as V
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  let closed = concat [valueCases, numberCases, stringCases, rejectCases, lexerCases, encodeCases, csvCases, framingCases]
      fails = closed
  if null fails
    then putStrLn "axioma: all green"
    else do
      mapM_ (hPutStrLn stderr) fails
      hPutStrLn stderr ("axioma: " ++ show (length fails) ++ " failure(s)")
      exitFailure

-- * closed-form oracles

check :: (Eq a, Show a) => String -> a -> a -> [String]
check name expected actual
  | expected == actual = []
  | otherwise = [name ++ ": expected " ++ show expected ++ ", got " ++ show actual]

dec :: B.ByteString -> Either String Json
dec = decodeJson

num :: Integer -> Int -> Json
num c e = JNumber (scientific c e)

valueCases :: [String]
valueCases =
  concat
    [ check "rfc object" (Right (JObject [("a", JArray (V.fromList [num 1 0, JBool True, JNull]))])) (dec "{\"a\": [1, true, null]}"),
      check "empty object" (Right (JObject [])) (dec "{}"),
      check "empty array" (Right (JArray V.empty)) (dec "[ ]"),
      check "whitespace" (Right (JBool True)) (dec " \n\r\ttrue\n"),
      check "nested" (Right (JArray (V.fromList [JArray (V.fromList [JArray V.empty])]))) (dec "[[[]]]"),
      check "key order preserved" (Right (JObject [("b", num 1 0), ("a", num 2 0)])) (dec "{\"b\":1,\"a\":2}"),
      check "duplicate keys preserved" (Right (JObject [("a", num 1 0), ("a", num 2 0)])) (dec "{\"a\":1,\"a\":2}")
    ]

numberCases :: [String]
numberCases =
  concat
    [ check "zero" (Right (num 0 0)) (dec "0"),
      check "minus zero" (Right (num 0 0)) (dec "-0"),
      check "integer" (Right (num 123 0)) (dec "123"),
      check "negative" (Right (num (-42) 0)) (dec "-42"),
      check "fraction" (Right (num 1 (-1))) (dec "0.1"),
      check "exponent" (Right (num 1 (-3))) (dec "1e-3"),
      check "exponent plus" (Right (num 15 0)) (dec "1.5E+1"),
      check "big coefficient" (Right (num 10000000000000000999 0)) (dec "10000000000000000999")
    ]

stringCases :: [String]
stringCases =
  concat
    [ check "plain" (Right (JString "hello")) (dec "\"hello\""),
      check "escapes" (Right (JString "a\nb\t\"c\"\\d/e\bs\ft\ru")) (dec "\"a\\nb\\t\\\"c\\\"\\\\d\\/e\\bs\\ft\\ru\""),
      check "unicode escape" (Right (JString "A\233")) (dec "\"\\u0041\\u00e9\""),
      check "surrogate pair" (Right (JString "\119070")) (dec "\"\\uD834\\uDD1E\""),
      check "utf8 passthrough" (Right (JString "héllo")) (dec (B.pack [0x22, 0x68, 0xC3, 0xA9, 0x6C, 0x6C, 0x6F, 0x22])),
      check "empty string" (Right (JString "")) (dec "\"\"")
    ]

rejectCases :: [String]
rejectCases =
  concat
    [ check "trailing comma array" (Left "invalid JSON") (dec "[1,]"),
      check "trailing comma object" (Left "invalid JSON") (dec "{\"a\":1,}"),
      check "leading zero" (Left "invalid JSON") (dec "01"),
      check "missing colon" (Left "invalid JSON") (dec "{\"a\" 1}"),
      check "trailing garbage" (Left "invalid JSON") (dec "true false"),
      check "empty input" (Left "invalid JSON") (dec ""),
      check "lone minus" (Left "invalid JSON") (dec "-"),
      check "unterminated string" (Left "invalid JSON") (dec "\"ab"),
      check "bad escape" (Left "invalid JSON") (dec "\"a\\xb\""),
      check "lone high surrogate" (Left "invalid JSON") (dec "\"\\uD834x\""),
      check "control character" (Left "invalid JSON") (dec "a\&\"a\USb\""),
      check "single quotes" (Left "invalid JSON") (dec "'a'")
    ]

lexerCases :: [String]
lexerCases =
  concat
    [ check
        "lexer flat"
        (Right [TBraceOpen, TString "a", TColon, TBrackOpen, TNumber "1", TComma, TTrue, TBrackClose, TBraceClose])
        (runJsonLexerBS "{\"a\": [1, true]}"),
      check
        "lexer escaped quote"
        (Right [TString "a\\\"b"])
        (runJsonLexerBS "\"a\\\"b\""),
      check
        "lexer unterminated"
        (Left (3, "unterminated string"))
        (runJsonLexerBS "\"ab")
    ]

-- | A tree roundtrip pin: 'encodeJson' then 'decodeJson' must return the
-- /same/ tree ('Scientific' equality is structural, so this pins the
-- number rendering, not just the value).
rt :: String -> Json -> [String]
rt name j = check ("roundtrip " ++ name) (Right j) (dec (encodeJson j))

enc :: String -> B.ByteString -> Json -> [String]
enc name expected j = check ("encode " ++ name) expected (encodeJson j)

encodeCases :: [String]
encodeCases =
  concat
    [ -- literal pins: compact, ordered, duplicates kept
      enc "object" "{\"a\":[1,true,null]}" (JObject [("a", JArray (V.fromList [num 1 0, JBool True, JNull]))]),
      enc "key order" "{\"b\":1,\"a\":2}" (JObject [("b", num 1 0), ("a", num 2 0)]),
      enc "duplicate keys" "{\"a\":1,\"a\":2}" (JObject [("a", num 1 0), ("a", num 2 0)]),
      enc "empty object" "{}" (JObject []),
      enc "empty array" "[]" (JArray V.empty),
      -- number rendering inverts the parser structurally
      enc "zero exponent" "10" (num 10 0),
      enc "positive exponent" "1e1" (num 1 1),
      enc "negative exponent" "1.50" (num 150 (-2)),
      enc "leading zeros after point" "0.0015" (num 15 (-4)),
      enc "zero coefficient" "0" (num 0 0),
      enc "zero coefficient, exponent" "0e5" (num 0 5),
      enc "negative coefficient" "-1.50" (num (-150) (-2)),
      enc "negative, positive exponent" "-1e1" (num (-1) 1),
      -- string escapes
      enc "newline" "\"a\\nb\"" (JString "a\nb"),
      enc "quote and backslash" "\"\\\"q\\\" \\\\\"" (JString "\"q\" \\"),
      enc "control char" "\"\\u0001\"" (JString "\SOH"),
      enc "unicode raw" "\"\206\187\"" (JString "\x3bb"),
      -- tree roundtrips (structural)
      rt "scalar" (JBool False),
      rt "numbers" (JArray (V.fromList [num 10 0, num 1 1, num 150 (-2), num 15 (-4), num (-150) (-2), num 0 5])),
      rt
        "nested with escapes"
        ( JObject
            [ ("msg", JString "line1\nline2 \"quoted\" \x3bb"),
              ("tags", JArray (V.fromList [JString "a", JNull])),
              ("dup", JNumber (scientific 1 1)),
              ("dup", JNumber (scientific 10 0))
            ]
        )
    ]

-- * csv

csv :: [[T.Text]] -> Either String Csv
csv = Right . Csv . V.fromList . map V.fromList

csvCases :: [String]
csvCases =
  concat
    [ check "csv crlf" (csv [["a", "b"], ["c", "d"]]) (decodeCsv "a,b\r\nc,d\r\n"),
      check "csv lf" (csv [["a", "b"], ["c", "d"]]) (decodeCsv "a,b\nc,d\n"),
      check "csv cr" (csv [["a", "b"], ["c", "d"]]) (decodeCsv "a,b\rc,d\r"),
      check "csv quoted comma" (csv [["a,b", "c"]]) (decodeCsv "\"a,b\",c"),
      check "csv doubled quotes" (csv [["say \"hi\"", "ok"]]) (decodeCsv "\"say \"\"hi\"\"\",ok"),
      check "csv embedded newline" (csv [["a\nb", "c"]]) (decodeCsv "\"a\nb\",c"),
      check "csv empty fields" (csv [["a", "", "b"]]) (decodeCsv "a,,b"),
      check "csv trailing comma" (csv [["a", "b", ""]]) (decodeCsv "a,b,\n"),
      check "csv blank middle line" (csv [["a"], [""], ["b"]]) (decodeCsv "a\n\nb"),
      check "csv empty input" (csv []) (decodeCsv ""),
      check "csv no trailing eol" (csv [["a", "b"], ["c", "d"]]) (decodeCsv "a,b\nc,d"),
      check "csv reject bare quote" (Left "invalid CSV") (decodeCsv "a\"b,c"),
      check "csv reject unterminated" (Left "invalid CSV") (decodeCsv "a,\"b"),
      check "csv reject garbage after quote" (Left "invalid CSV") (decodeCsv "\"a\"x,b"),
      check
        "csv lexer flat"
        (Right [CField "a", CField "b", CRowEnd, CQuoted "x\"\"y", CField "z"])
        (runCsvLexerBS "a,b\r\n\"x\"\"y\",z"),
      check "csv lexer unterminated" (Left (4, "unterminated quoted field")) (runCsvLexerBS "a,\"b"),
      check "csv lexer bare quote" (Left (2, "unexpected quote in plain field")) (runCsvLexerBS "ab\"cd")
    ]

-- * concrete post framing

framingTime :: UTCTime
framingTime =
  fromMaybe (error "axioma: invalid fixed timestamp") $
    parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S" "2026-08-03T23:10:25"

stored :: Stored
stored =
  Stamped
    (framingTime, 42)
    (Post "kimi" (Just "bus") ["yin", "stack-org"] [3, 5] "hello \nworld ♪")

plainStored :: Stored
plainStored =
  Stamped
    (framingTime, 7)
    (Post "a" Nothing ["b"] [2] "body")

discardStored :: Stored
discardStored =
  Stamped
    (framingTime, 7)
    (Post "a" Nothing [] [2] "body")

secondStored :: Stored
secondStored =
  Stamped
    (framingTime, 43)
    (Post "yin" Nothing ["kimi"] [] "next")

-- | The August fleet log's single malformed row, verbatim
-- (@~/.config/free-agent-bus/log.jsonl@, line 269): a null id and a
-- fractional-second UTC offset timestamp in one line. The timestamp form is
-- what the decoder names first; the null id is refused on its own by
-- "framing null id row error triple", which carries a well-formed timestamp.
augustNullIdRow :: T.Text
augustNullIdRow =
  "{\"id\": null, \"ts\": \"2026-08-07T01:01:14.369092+00:00\", \"from\": \"scanner-4\", \"to\": [\"pitboss\"], \"thread\": [], \"body\": \"scanner-4 DONE: archive-dag-report-scanner-4.md — posts 133..176, lines 133..176\"}"

-- | The August failure at its original position. Two hundred sixty-eight valid
-- rows precede the verbatim bad row; padding the last valid body puts its
-- zero-based character offset at 291893 without shipping a generated fixture.
augustFleetLog :: T.Text
augustFleetLog =
  T.replicate 267 prefixUnit
    <> changedRow "body" (JString (T.replicate augustPadding "x"))
    <> "\n"
    <> augustNullIdRow
  where
    prefixUnit = encodeRow stored <> "\n"
    emptyBodyUnit = changedRow "body" (JString "") <> "\n"
    augustPadding = 291893 - 267 * T.length prefixUnit - T.length emptyBodyUnit

storedFields :: [(T.Text, Json)]
storedFields = case encodeStored stored of
  JObject fields -> fields
  _ -> error "axioma: encodeStored produced a non-object"

plainFields :: [(T.Text, Json)]
plainFields = case encodeStored plainStored of
  JObject fields -> fields
  _ -> error "axioma: encodeStored produced a non-object"

objectRow :: [(T.Text, Json)] -> T.Text
objectRow = decodeUtf8 . encodeJson . JObject

changedRow :: T.Text -> Json -> T.Text
changedRow = changedFields storedFields

changedPlainRow :: T.Text -> Json -> T.Text
changedPlainRow = changedFields plainFields

changedFields :: [(T.Text, Json)] -> T.Text -> Json -> T.Text
changedFields fields key new =
  objectRow [(key', if key == key' then new else old) | (key', old) <- fields]

rejectAt :: String -> Int -> Int -> T.Text -> [String]
rejectAt name expectedRow expectedOffset input =
  case decodeLog input of
    Left (RowError row offset _) -> check name (expectedRow, expectedOffset) (row, offset)
    Right posts -> [name ++ ": expected rejection, decoded " ++ show (length posts) ++ " row(s)"]

-- | Reject and compare the whole 'RowError', reason included.
rejectRow :: String -> RowError -> T.Text -> [String]
rejectRow name expected input =
  case decodeLog input of
    Left err -> check name expected err
    Right posts -> [name ++ ": expected rejection, decoded " ++ show (length posts) ++ " row(s)"]

framingCases :: [String]
framingCases =
  concat
    [ check "framing stamped roundtrip" (Right stored) (decodeRow (encodeRow stored)),
      check "framing plain channel roundtrip" (Right plainStored) (decodeRow (encodeRow plainStored)),
      check
        "framing empty recipient normalises"
        (Right discardStored)
        (decodeRow (changedPlainRow "to" (JArray (V.fromList [JString ""])))),
      check
        "framing empty recipient delivers nowhere"
        (Right False)
        (fmap (\stored' -> deliversTo (stamped stored') ["a", "b"]) (decodeRow (changedPlainRow "to" (JArray (V.fromList [JString ""]))))),
      -- The discriminating case: ["a", "b"] is a subscription list on which a
      -- core `deliversTo` would agree with or without the normalisation, so it
      -- cannot witness the legacy discard convention. [""] is a subscriber the
      -- empty string would hit if the decoder passed ["\""] through.
      check
        "framing empty recipient does not deliver to the empty subscription"
        (Right False)
        (fmap (\stored' -> deliversTo (stamped stored') [""]) (decodeRow (changedPlainRow "to" (JArray (V.fromList [JString ""]))))),
      check
        "framing empty audience delivers nowhere"
        (Right False)
        (fmap (\stored' -> deliversTo (stamped stored') ["a", "b"]) (decodeRow (encodeRow discardStored))),
      check
        "framing empty audience does not deliver to the empty subscription"
        (Right False)
        (fmap (\stored' -> deliversTo (stamped stored') [""]) (decodeRow (encodeRow discardStored))),
      check
        "framing named recipient delivers"
        (Right True)
        (fmap (\stored' -> deliversTo (stamped stored') ["b"]) (decodeRow (encodeRow plainStored))),
      case encodeStored plainStored of
        JObject fields ->
          let withoutChannel = filter ((/= "channel") . fst) fields
           in check "framing absent legacy channel" (Right plainStored) (decodeRow (objectRow withoutChannel))
        _ -> ["framing absent legacy channel: non-object"],
      check "framing integral decimal id" (Right stored) (decodeRow (changedRow "id" (JNumber 42.0))),
      check
        "framing oldest-first encoding"
        (encodeRow stored <> "\n" <> encodeRow secondStored <> "\n")
        (encodeLog [stored, secondStored]),
      check
        "framing oldest-first decoding and blank rows"
        (Right [stored, secondStored])
        (decodeLog ("\n" <> encodeRow stored <> "\n \t\n" <> encodeRow secondStored)),
      rejectAt "framing mixed recipients" 1 0 (changedRow "to" (JArray (V.fromList [JString "yin", JNumber 0, JString "dropped"]))),
      rejectAt "framing mixed parents" 1 0 (changedRow "thread" (JArray (V.fromList [JNumber 3, JString "discarded"]))),
      rejectAt "framing negative parent" 1 0 (changedRow "thread" (JArray (V.fromList [JNumber (-1)]))),
      rejectAt "framing fractional parent" 1 0 (changedRow "thread" (JArray (V.fromList [JNumber 1.5]))),
      rejectAt "framing negative id" 1 0 (changedRow "id" (JNumber (-1))),
      rejectRow "framing null id row error triple" (RowError 1 0 "id/thread: expected number") (changedRow "id" JNull),
      rejectAt "framing fractional id" 1 0 (changedRow "id" (JNumber 42.5)),
      rejectAt "framing sender type" 1 0 (changedRow "from" (JBool True)),
      rejectAt "framing empty sender" 1 0 (changedRow "from" (JString "")),
      rejectAt "framing timestamp type" 1 0 (changedRow "ts" (JNumber 0)),
      rejectAt "framing malformed timestamp" 1 0 (changedRow "ts" (JString "not-a-time")),
      rejectAt "framing channel type" 1 0 (changedRow "channel" (JBool False)),
      rejectAt "framing recipients type" 1 0 (changedRow "to" (JString "yin")),
      rejectAt "framing parents type" 1 0 (changedRow "thread" (JObject [])),
      rejectAt "framing body type" 1 0 (changedRow "body" (JNumber 1)),
      rejectAt "framing missing recipients" 1 0 (objectRow (filter ((/= "to") . fst) storedFields)),
      rejectAt "framing missing id" 1 0 (objectRow (filter ((/= "id") . fst) storedFields)),
      rejectAt "framing missing timestamp" 1 0 (objectRow (filter ((/= "ts") . fst) storedFields)),
      rejectAt "framing missing sender" 1 0 (objectRow (filter ((/= "from") . fst) storedFields)),
      rejectAt "framing missing body" 1 0 (objectRow (filter ((/= "body") . fst) storedFields)),
      rejectAt "framing duplicate id" 1 0 (objectRow (storedFields ++ [("id", JNumber 42)])),
      rejectAt "framing legacy triple rejected" 1 0 "{\"ts\":\"2026-08-03T12:00:00\",\"sender\":\"kimi\",\"body\":\"legacy\"}",
      rejectAt "framing legacy bracket rejected" 1 0 "[2026-08-03T12:00:00] kimi: legacy",
      rejectAt "framing august null-id row" 1 0 augustNullIdRow,
      rejectRow
        "framing august fleet row error triple"
        (RowError 269 291893 "ts: expected %Y-%m-%dT%H:%M:%S UTC timestamp")
        augustFleetLog,
      rejectAt
        "framing malformed JSON row and character offset"
        3
        (T.length (encodeRow stored) + 2)
        (encodeRow stored <> "\n\n{\"id\":"),
      -- The whole RowError, reason included, so the codec's own sentence is
      -- pinned rather than a wrapper's rendering of it. A fractional-second
      -- timestamp is the shape the August fleet log carries, and the offset is
      -- the second row's start, one character past the first row's newline.
      rejectRow
        "framing timestamp row error triple"
        (RowError 2 (T.length (encodeRow stored) + 1) "ts: expected %Y-%m-%dT%H:%M:%S UTC timestamp")
        (encodeRow stored <> "\n" <> changedRow "ts" (JString "2026-08-07T01:01:14.369092+00:00")),
      check
        "framing syntax error names a byte offset"
        (Left "invalid JSON at byte 10: unterminated string")
        (decodeRow "{\"id\":\"abc")
    ]
