-- | Selective materialization over the fast path.
--
-- "Circuit.Parser.Json.Lexer" tokenizes; this module turns token
-- streams into trees — the fast path's answer to 'decodeJson'. Two
-- consumers:
--
-- * 'decodeJsonFast': the whole document, materialized from tokens.
--   Same grammar, same 'unescape', same 'bsToScientific' as the
--   combinator path — the tree is identical, and it is an order of
--   magnitude cheaper to build (coffee/loom/transcripts-perf.md).
--
-- * 'fields': a selective object walk — keep only the wanted keys,
--   materializing only their values ('Want'), skipping the rest as
--   token spans. The walk that makes payload-heavy lines cheap: an
--   extraction that reads five fields of a line never pays for the
--   rest.
--
-- Leniency versus 'decodeJson', one class: inside SKIPPED subtrees,
-- string escapes go unvalidated (a bad escape there fails 'decodeJson'
-- but passes a skipping walk). Everything materialized is validated
-- exactly — escapes by 'unescape', numbers by 'validNumber'.
module Circuit.Parser.Json.Select
  ( -- * selective materialization
    Want (..),
    fields,

    -- * full materialization
    treeOf,
    decodeJsonFast,

    -- * pieces
    skipValue,
    validNumber,
  )
where

import Circuit.Parser.Json (bsToScientific)
import Circuit.Parser.Json.Lexer (JsonToken (..), runJsonLexerBS)
import Circuit.Parser.Json.Unescape (unescape)
import Circuit.Parser.Json.Value (Json (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Vector qualified as V

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString.Char8 qualified as C

-- | What to do with a wanted key's value: keep it if it has the kind
-- the consumer reads (the same lookup-then-constructor test a fully
-- decoded tree would get), skip it otherwise. Kept or not, the first
-- occurrence of a key shadows later duplicates, as 'lookup' does on
-- the combinator path's association list.
data Want
  = -- | keep if a string ('unescape'-decoded)
    WStr
  | -- | keep if a number (strict grammar via 'validNumber', then
    -- 'bsToScientific')
    WNum
  | -- | keep if a bool
    WBool
  | -- | keep, fully materialized ('treeOf')
    WTree
  | -- | keep if an object, pruned recursively
    WObj [(Text, Want)]

-- | Walk object pairs (the token after the opening brace on), keeping
-- first occurrences of wanted keys. Returns the kept pairs in document
-- order and the tokens after the closing brace. Grammar as the
-- combinator path's @jobject@: pairs are string-colon-value, a comma
-- commits to another pair — a trailing comma is a parse failure.
--
-- >>> Right (TBraceOpen : ts) = runJsonLexerBS (C.pack "{\"a\":1,\"big\":[1,2,3],\"b\":\"x\"}")
-- >>> fields [("a", WNum), ("b", WStr)] ts
-- Just ([("a",JNumber 1.0),("b",JString "x")],[])
-- >>> fields [("big", WTree)] ts
-- Just ([("big",JArray [JNumber 1.0,JNumber 2.0,JNumber 3.0])],[])
fields :: [(Text, Want)] -> [JsonToken] -> Maybe ([(Text, Json)], [JsonToken])
fields ws = pair [] []
  where
    pair _ acc (TBraceClose : ts) = Just (reverse acc, ts)
    pair seen acc (TString k : TColon : ts) = do
      k' <- rightToMaybe (unescape k)
      case lookup k' ws of
        Just w
          | k' `elem` seen -> skipValue ts >>= close seen acc
          | otherwise -> do
              (mj, ts') <- capture w ts
              close (k' : seen) (maybe acc (\j -> (k', j) : acc) mj) ts'
        Nothing -> skipValue ts >>= close seen acc
    pair _ _ _ = Nothing
    close seen acc (TComma : ts) = case ts of
      (TString _ : TColon : _) -> pair seen acc ts
      _ -> Nothing
    close seen acc ts = pair seen acc ts

-- | One wanted value: kept when the kind matches, skipped (and
-- omitted) otherwise — a consumer's accessor reads a wrong-kinded
-- value and an absent one identically.
capture :: Want -> [JsonToken] -> Maybe (Maybe Json, [JsonToken])
capture WStr (TString s : ts) = (\t -> (Just (JString t), ts)) <$> rightToMaybe (unescape s)
capture WNum (TNumber s : ts)
  | validNumber s = Just (Just (JNumber (bsToScientific s)), ts)
  | otherwise = Nothing
capture WBool (TTrue : ts) = Just (Just (JBool True), ts)
capture WBool (TFalse : ts) = Just (Just (JBool False), ts)
capture WTree ts = (\(j, ts') -> (Just j, ts')) <$> treeOf ts
capture (WObj ws) (TBraceOpen : ts) = (\(ps, ts') -> (Just (JObject ps), ts')) <$> fields ws ts
capture _ ts = (,) Nothing <$> skipValue ts

-- | A full subtree, materialized. Built from the tokens with the same
-- 'unescape' and 'bsToScientific' as the combinator path: the tree is
-- identical, duplicates and order included.
treeOf :: [JsonToken] -> Maybe (Json, [JsonToken])
treeOf (TString s : ts) = (\t -> (JString t, ts)) <$> rightToMaybe (unescape s)
treeOf (TNumber s : ts)
  | validNumber s = Just (JNumber (bsToScientific s), ts)
  | otherwise = Nothing
treeOf (TTrue : ts) = Just (JBool True, ts)
treeOf (TFalse : ts) = Just (JBool False, ts)
treeOf (TNull : ts) = Just (JNull, ts)
treeOf (TBraceOpen : ts) = objPairs ts
  where
    objPairs (TBraceClose : ts') = Just (JObject [], ts')
    objPairs ts'@(TString _ : TColon : _) = do
      (p, ts'') <- pairOf ts'
      afterPair [p] ts''
    objPairs _ = Nothing
    afterPair acc (TBraceClose : ts') = Just (JObject (reverse acc), ts')
    afterPair acc (TComma : ts') = do
      (p, ts'') <- pairOf ts'
      afterPair (p : acc) ts''
    afterPair _ _ = Nothing
    pairOf (TString k : TColon : ts') = do
      k' <- rightToMaybe (unescape k)
      (v, ts'') <- treeOf ts'
      pure ((k', v), ts'')
    pairOf _ = Nothing
treeOf (TBrackOpen : ts) = elems ts
  where
    elems (TBrackClose : ts') = Just (JArray V.empty, ts')
    elems ts' = do
      (v, ts'') <- treeOf ts'
      afterElem [v] ts''
    afterElem acc (TBrackClose : ts') = Just (JArray (V.fromList (reverse acc)), ts')
    afterElem acc (TComma : ts') = do
      (v, ts'') <- treeOf ts'
      afterElem (v : acc) ts''
    afterElem _ _ = Nothing
treeOf _ = Nothing

-- | Parse a JSON document over the fast path: tokenize, materialize
-- with 'treeOf', and require the input to end there — the same
-- grammar as 'decodeJson', the same tree.
--
-- >>> decodeJsonFast (C.pack "{\"a\": [1, true]}")
-- Right (JObject [("a",JArray [JNumber 1.0,JBool True])])
--
-- >>> decodeJsonFast (C.pack "[1,]")
-- Left "invalid JSON"
decodeJsonFast :: ByteString -> Either String Json
decodeJsonFast s = case runJsonLexerBS s of
  Left (_, e) -> Left e
  Right ts -> case treeOf ts of
    Just (j, []) -> Right j
    _ -> Left "invalid JSON"

-- | Skip one value, grammar-checked, building nothing. Strings in
-- skipped subtrees keep their escapes unvalidated — the leniency the
-- module header promises.
skipValue :: [JsonToken] -> Maybe [JsonToken]
skipValue (TBraceOpen : ts) = snd <$> fields [] ts
skipValue (TBrackOpen : ts) = skipElems ts
skipValue (TString _ : ts) = Just ts
skipValue (TNumber s : ts)
  | validNumber s = Just ts
  | otherwise = Nothing
skipValue (TTrue : ts) = Just ts
skipValue (TFalse : ts) = Just ts
skipValue (TNull : ts) = Just ts
skipValue _ = Nothing

skipElems :: [JsonToken] -> Maybe [JsonToken]
skipElems (TBrackClose : ts) = Just ts
skipElems ts = skipValue ts >>= afterElem
  where
    afterElem (TBrackClose : ts') = Just ts'
    afterElem (TComma : ts') = skipValue ts' >>= afterElem
    afterElem _ = Nothing

-- | The JSON number grammar at byte level:
-- @-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?@. The lexer's
-- number span is the looser @[-+0-9.eE]*@; slices are re-validated so
-- a number the combinator path rejects fails here too.
--
-- >>> validNumber (C.pack "123")
-- True
-- >>> validNumber (C.pack "01")
-- False
-- >>> validNumber (C.pack "-1.5e+10")
-- True
validNumber :: ByteString -> Bool
validNumber s0 = maybe False B.null (intPart (stripMinus s0) >>= fracPart >>= expPart)
  where
    stripMinus s = case B.uncons s of
      Just (45, r) -> r
      _ -> s
    intPart s = case B.uncons s of
      Just (48, r) -> Just r
      Just (w, r) | w >= 49 && w <= 57 -> Just (B.dropWhile isDigit r)
      _ -> Nothing
    fracPart s = case B.uncons s of
      Just (46, r) -> digits1 r
      _ -> Just s
    expPart s = case B.uncons s of
      Just (w, r)
        | w == 101 || w == 69 ->
            case B.uncons r of
              Just (w', r') | w' == 43 || w' == 45 -> digits1 r'
              _ -> digits1 r
      _ -> Just s
    digits1 s = case B.uncons s of
      Just (w, _) | isDigit w -> Just (B.dropWhile isDigit s)
      _ -> Nothing
    isDigit w = w >= 48 && w <= 57

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just
