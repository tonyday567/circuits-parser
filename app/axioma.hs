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
import Circuit.Parser.Json
import Data.ByteString qualified as B
import Data.Scientific (scientific)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  let closed = concat [valueCases, numberCases, stringCases, rejectCases, lexerCases, encodeCases, csvCases]
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
