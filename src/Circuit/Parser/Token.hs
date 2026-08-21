{-# LANGUAGE OverloadedStrings #-}

module Circuit.Parser.Token
  ( -- Tokenization
    tokenize,
    tokenizeLoop,
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
    takeTopN,
  )
where

import Circuit.Parser (Parser, These (..), Uncons (..), char, runParserIdentity, satisfy, some, (<|>))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Functor.Identity (Identity)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

-- ============================================================================
-- Token Patterns
-- ============================================================================

lowerLetter :: (Uncons f Char) => Parser Identity f Char Char
lowerLetter = satisfy isAsciiLower

upperLetter :: (Uncons f Char) => Parser Identity f Char Char
upperLetter = satisfy isAsciiUpper

letter :: (Uncons f Char) => Parser Identity f Char Char
letter = lowerLetter <|> upperLetter

digit :: (Uncons f Char) => Parser Identity f Char Char
digit = satisfy isDigit

word :: (Uncons f Char) => Parser Identity f Char [Char]
word = some letter

number :: (Uncons f Char) => Parser Identity f Char [Char]
number = some digit

punctuation :: (Uncons f Char) => Parser Identity f Char Char
punctuation = foldr1 (<|>) (map char ",.;:!?'\"-()[]{}@#$%&*+=<>/\\\\|~`")

token :: (Uncons f Char) => Parser Identity f Char [Char]
token = word <|> number <|> fmap (: []) punctuation

-- ============================================================================
-- Core Tokenizer
-- ============================================================================

-- | Tokenize text into a list of tokens using Circuit.Parser patterns.
tokenize :: Text -> [Text]
tokenize input =
  let str = Text.unpack input
      tokens = tokenizeLoop str []
   in map Text.pack tokens

-- | Recursively extract tokens using runParser
tokenizeLoop :: String -> [String] -> [String]
tokenizeLoop [] acc = reverse acc
tokenizeLoop s acc =
  case runParserIdentity token s of
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

-- ============================================================================
-- Vocabulary Building
-- ============================================================================

data Vocabulary = Vocabulary
  { vocabTokenToIndex :: !(Map.Map Text Int),
    vocabIndexToToken :: !(IntMap.IntMap Text),
    vocabSize :: !Int
  }
  deriving (Eq, Show)

buildVocabulary :: [Text] -> Vocabulary
buildVocabulary tokens =
  let (fwd, rev, finalIdx) = foldl' insertUnique (Map.empty, IntMap.empty, 0) tokens
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
lookupIndex tok vocab = Map.lookup tok (vocabTokenToIndex vocab)

lookupToken :: Int -> Vocabulary -> Maybe Text
lookupToken idx vocab = IntMap.lookup idx (vocabIndexToToken vocab)

vocabularySize :: Vocabulary -> Int
vocabularySize = vocabSize

-- ============================================================================
-- Vocabulary Filtering
-- ============================================================================

filterVocabulary :: (Text -> Bool) -> Vocabulary -> Vocabulary
filterVocabulary predicate vocab =
  let filteredFwd =
        Map.filterWithKey
          (\tok _ -> predicate tok)
          (vocabTokenToIndex vocab)
      tokenList = Map.keys filteredFwd
      (newFwd, newRev, newSize) =
        foldl'
          ( \(f, r, i) tok ->
              (Map.insert tok i f, IntMap.insert i tok r, i + 1)
          )
          (Map.empty, IntMap.empty, 0)
          tokenList
   in Vocabulary newFwd newRev newSize

takeTopN :: Int -> Vocabulary -> Vocabulary
takeTopN n vocab =
  let tokensToKeep =
        [ tok
        | i <- [0 .. min (n - 1) (vocabSize vocab - 1)],
          Just tok <- [IntMap.lookup i (vocabIndexToToken vocab)]
        ]
      newFwd = Map.fromList (zip tokensToKeep [0 ..])
      newRev = IntMap.fromList (zip [0 ..] tokensToKeep)
   in Vocabulary newFwd newRev (length tokensToKeep)
