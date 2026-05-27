{-# LANGUAGE OverloadedStrings #-}

module Circuit.Parser.Tokenizer
  ( -- Tokenization
    tokenize,
    tokenizeLoop,
    prettifyTokens,
    -- Token patterns
    lowerLetter,
    upperLetter,
    letter,
    digit,
    word,
    number,
    punctuation,
    token,
    -- Vocabulary
    Vocabulary (..),
    buildVocabulary,
    lookupIndex,
    lookupToken,
    vocabularySize,
    filterVocabulary,
    takeFirstN,
    prettifyVocabulary,
  )
where

import Circuit.Parser (Parser (..), These (..), Uncons (..), char, runParser, satisfy, some, (<|>))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Harpie.Array (Array)
import Harpie.Array qualified as A
import Prettyprinter (pretty)

-- ============================================================================
-- Token Patterns
-- ============================================================================

lowerLetter :: (Uncons f Char) => Parser f Char Char
lowerLetter = satisfy isAsciiLower

upperLetter :: (Uncons f Char) => Parser f Char Char
upperLetter = satisfy isAsciiUpper

letter :: (Uncons f Char) => Parser f Char Char
letter = lowerLetter <|> upperLetter

digit :: (Uncons f Char) => Parser f Char Char
digit = satisfy isDigit

word :: (Uncons f Char) => Parser f Char [Char]
word = some letter

number :: (Uncons f Char) => Parser f Char [Char]
number = some digit

punctuation :: (Uncons f Char) => Parser f Char Char
punctuation = foldr1 (<|>) (map char ",.;:!?'\"-()[]{}@#$%&*+=<>/\\\\|~`")

token :: (Uncons f Char) => Parser f Char [Char]
token = word <|> number <|> fmap (: []) punctuation

-- ============================================================================
-- Core Tokenizer
-- ============================================================================

-- | Tokenize text into an array of tokens using Circuit.Parser patterns
tokenize :: Text -> Array Text
tokenize input =
  let str = Text.unpack input
      tokens = tokenizeLoop str []
   in if null tokens
        then A.array [0] []
        else A.array [length tokens] (map Text.pack tokens)

-- | Recursively extract tokens using runParser
tokenizeLoop :: String -> [String] -> [String]
tokenizeLoop [] acc = reverse acc
tokenizeLoop s acc =
  case runParser token s of
    These tok rest ->
      if null tok
        then tokenizeLoop rest acc
        else tokenizeLoop rest (tok : acc)
    This tok ->
      if null tok
        then reverse acc
        else reverse (tok : acc)
    That _ ->
      case s of
        (_ : rest) -> tokenizeLoop rest acc

-- | Pretty-print token array for display
prettifyTokens :: Array Text -> String
prettifyTokens arr = show (pretty arr)

-- ============================================================================
-- Vocabulary Building
-- ============================================================================

data Vocabulary = Vocabulary
  { tokenToIndex :: !(Map.Map Text Int),
    indexToToken :: !(IntMap.IntMap Text),
    size :: !Int
  }
  deriving (Eq, Show)

buildVocabulary :: Array Text -> Vocabulary
buildVocabulary tokens =
  let tokenList = toList tokens
      (fwd, rev, finalIdx) = foldl' insertUnique (Map.empty, IntMap.empty, 0) tokenList
   in Vocabulary fwd rev finalIdx
  where
    insertUnique (fwd, rev, idx) tok =
      if Map.member tok fwd
        then (fwd, rev, idx)
        else
          ( Map.insert tok idx fwd,
            IntMap.insert idx tok rev,
            idx + 1
          )

-- ============================================================================
-- Vocabulary Lookups
-- ============================================================================

lookupIndex :: Text -> Vocabulary -> Maybe Int
lookupIndex tok vocab = Map.lookup tok (tokenToIndex vocab)

lookupToken :: Int -> Vocabulary -> Maybe Text
lookupToken idx vocab = IntMap.lookup idx (indexToToken vocab)

vocabularySize :: Vocabulary -> Int
vocabularySize = size

-- ============================================================================
-- Vocabulary Filtering
-- ============================================================================

filterVocabulary :: (Text -> Bool) -> Vocabulary -> Vocabulary
filterVocabulary predicate vocab =
  let filteredFwd =
        Map.filterWithKey
          (\tok _ -> predicate tok)
          (tokenToIndex vocab)
      tokenList = Map.keys filteredFwd
      (newFwd, newRev, newSize) =
        foldl'
          ( \(f, r, i) tok ->
              (Map.insert tok i f, IntMap.insert i tok r, i + 1)
          )
          (Map.empty, IntMap.empty, 0)
          tokenList
   in Vocabulary newFwd newRev newSize

takeFirstN :: Int -> Vocabulary -> Vocabulary
takeFirstN n vocab =
  let tokensToKeep =
        [ tok
        | i <- [0 .. min (n - 1) (size vocab - 1)],
          Just tok <- [IntMap.lookup i (indexToToken vocab)]
        ]
      newFwd = Map.fromList (zip tokensToKeep [0 ..])
      newRev = IntMap.fromList (zip [0 ..] tokensToKeep)
   in Vocabulary newFwd newRev (length tokensToKeep)

-- ============================================================================
-- Vocabulary Display
-- ============================================================================

prettifyVocabulary :: Vocabulary -> String
prettifyVocabulary vocab =
  let vocabList =
        [ (idx, tok)
        | idx <- [0 .. size vocab - 1],
          Just tok <- [IntMap.lookup idx (indexToToken vocab)]
        ]
   in unlines $ map (\(i, tok) -> show i ++ ": " ++ Text.unpack tok) vocabList
