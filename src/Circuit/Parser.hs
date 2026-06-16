{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A parser combinator library built on Circuit.
--
-- @Parser f s a@ consumes elements of type @s@ from a stream of type @f@
-- (decomposed via 'Uncons') and produces a result of type @a@, with
-- progress-aware failure via 'These'.
--
-- The stream type @f@ is opaque — parsers only see one element at a time
-- through 'uncons'. This makes the parser coinductive and polymorphic over
-- any stream-like type: @String@, @Text@, @ByteString@, etc.
--
-- @
--   'This' a    — consumed everything, final result
--   'That' f    — no progress, stream intact (for backtracking)
--   'These' a f — consumed some, result + remainder
-- @
--
-- >>> runParser (char 'a') "abc"
-- These 'a' "bc"
--
-- >>> runParser (char 'x') "abc"
-- That "abc"
module Circuit.Parser
  ( -- * Type
    Parser (..),
    These (..),

    -- * Running
    runParser,
    runParserMaybe,
    runParserError,

    -- * Building
    satisfy,
    char,
    string,
    anyToken,
    filterP,

    -- * Repetition
    many,
    some,

    -- * Capture
    captured,
    skipWhile,

    -- * Stream decomposition
    Uncons (..),
    HasEmpty (..),
    HasLength (..),

    -- * These helpers
    asThese,
    asMaybe',
    asEither,

    -- * Choice
    empty,
    (<|>),

    -- * These threading
    thenThese,

    -- * Additional combinators
    optional,
    skipMany,
    takeRest,
    satisfyAscii,
    withOption,
    chainr,
    count,
    sepBy,
    sepBy1,
    try,
    endOfInput,
    lineEnd,
  )
where

import Circuit (Circuit (..), reify)
import Control.Monad (void)
import Data.Functor (($>))
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.These (These (..))
import Data.Word (Word8)

-- | Types that have an empty/zero value.
class HasEmpty f where
  emptyF :: f

instance HasEmpty [a] where emptyF = []

instance HasEmpty B.ByteString where emptyF = B.empty

instance HasEmpty Text where emptyF = T.empty

-- | Types whose length can be measured and prefix-taken.
class HasLength f where
  streamLength :: f -> Int
  streamTake :: Int -> f -> f

instance HasLength [a] where
  streamLength = length
  streamTake = take

instance HasLength B.ByteString where
  streamLength = B.length
  streamTake = B.take

-- | Typeclass for deconstructing input streams into head and tail.
class (HasEmpty f) => Uncons f s where
  uncons :: f -> These s f

newtype Parser f s a = Parser
  { unParser :: Circuit (->) Either f (These a f)
  }

-- | Run a parser on a stream, returning the 'These' result.
runParser :: Parser f s a -> f -> These a f
runParser = reify . unParser

instance Uncons [a] a where
  uncons [] = That []
  uncons (x : xs) = These x xs

instance Uncons B.ByteString Char where
  uncons bs
    | B.null bs = That bs
    | otherwise = These (toEnum (fromIntegral (B.head bs)) :: Char) (B.tail bs)

instance Uncons B.ByteString Word8 where
  uncons bs
    | B.null bs = That bs
    | otherwise = These (B.head bs) (B.tail bs)

instance Uncons Text Char where
  uncons t
    | T.null t = That t
    | otherwise = These (T.head t) (T.tail t)

-- | Consume and return the next element, or 'That' if the stream is empty.
--
-- @
-- runParser anyToken \"abc\" = These \'a\' \"bc\"
-- runParser anyToken \"\" = That \"\"
-- @
anyToken :: (Uncons f s) => Parser f s s
anyToken = Parser $ Lift $ \f -> case uncons f of
  That _ -> That f
  These s f' -> These s f'
  This _ -> That f

-- | Consume one element if it satisfies the predicate.
--
-- >>> runParser (satisfy (> 'a')) "bcd"
-- These 'b' "cd"
-- >>> runParser (satisfy (> 'a')) "abc"
-- That "abc"
satisfy :: (Uncons f s) => (s -> Bool) -> Parser f s s
satisfy p = Parser $ Lift $ \f -> case uncons f of
  That _ -> That f
  These s f'
    | p s -> These s f'
    | otherwise -> That f
  This _ -> That f

-- | Match a specific element.
--
-- >>> runParser (char 'x') "xyz"
-- These 'x' "yz"
char :: (Uncons f s, Eq s) => s -> Parser f s s
char c = satisfy (== c)

-- | Match a sequence of elements.
--
-- >>> runParser (string "ab") "abc"
-- These "ab" "c"
string :: (Uncons f s, Eq s) => [s] -> Parser f s [s]
string = traverse char

-- | Keep only successes matching the predicate.
filterP :: (Uncons f s) => Parser f s a -> (a -> Bool) -> Parser f s a
filterP (Parser p) f = Parser $ Lift $ \s ->
  case reify p s of
    This a
      | f a -> This a
      | otherwise -> That s
    That s' -> That s'
    These a s'
      | f a -> These a s'
      | otherwise -> That s

-- | Zero or more repetitions. Accumulates results until failure.
--
-- Stops on 'That' (no progress), returning what was accumulated.
--
-- >>> runParser (many (char 'a')) "aaab"
-- These "aaa" "b"
-- >>> runParser (many (char 'a')) "xyz"
-- These "" "xyz"
many :: Parser f s a -> Parser f s [a]
many p = Parser $ Lift $ \s -> go s []
  where
    go s acc = case runParser p s of
      This a -> This (reverse (a : acc))
      These a s' -> go s' (a : acc)
      That s' -> These (reverse acc) s'

-- | One or more repetitions. Fails if the first parse fails.
--
-- >>> runParser (some (char 'a')) "aaab"
-- These "aaa" "b"
-- >>> runParser (some (char 'a')) "xyz"
-- That "xyz"
some :: (Uncons f s) => Parser f s a -> Parser f s [a]
some p = (:) <$> p <*> many p

-- | Thread state through 'These' results.
thenThese :: (HasEmpty f) => These a f -> (a -> f -> These b f) -> These b f
thenThese (This a) f = f a emptyF
thenThese (That s) _ = That s
thenThese (These a s) f = f a s

-- | Extract value from a parse result, erroring on failure.
asThese :: These a f -> a
asThese (This a) = a
asThese (These a _) = a
asThese (That _) = error "parse failed"

-- | Convert a parse result to Maybe.
asMaybe' :: These a f -> Maybe a
asMaybe' (This a) = Just a
asMaybe' (These a _) = Just a
asMaybe' (That _) = Nothing

-- | Convert a parse result to Either (failure returns Left with the leftover stream).
asEither :: These a f -> Either f a
asEither (This a) = Right a
asEither (These a _) = Right a
asEither (That f) = Left f

-- | Run a parser and extract the result as Maybe.
runParserMaybe :: Parser f s a -> f -> Maybe a
runParserMaybe p f = asMaybe' (runParser p f)

-- | Run a parser and extract the result, erroring on failure.
runParserError :: Parser f s a -> f -> a
runParserError p f = asThese (runParser p f)

instance Functor (Parser f s) where
  fmap f (Parser p) = Parser $ Lift $ \s ->
    case reify p s of
      This a -> This (f a)
      That s' -> That s'
      These a s' -> These (f a) s'

instance (Uncons f s) => Applicative (Parser f s) where
  pure a = Parser $ Lift $ \f -> These a f
  Parser pf <*> Parser pa = Parser $ Lift $ \s ->
    case reify pf s of
      This f ->
        This f
          `thenThese` ( \_ s' -> case reify pa s' of
                          This a' -> This (f a')
                          That _ -> That s
                          These a' s''' -> These (f a') s'''
                      )
      That s' -> That s'
      These f s' ->
        These f s'
          `thenThese` ( \_ s'' -> case reify pa s'' of
                          This a' -> This (f a')
                          That _ -> That s
                          These a' s''' -> These (f a') s'''
                      )
  Parser p1 *> Parser p2 = Parser $ Lift $ \s ->
    case reify p1 s of
      This _ -> reify p2 emptyF
      That s' -> That s'
      These _ s' -> case reify p2 s' of
        That _ -> That s
        result -> result
  Parser p1 <* Parser p2 = Parser $ Lift $ \s ->
    case reify p1 s of
      This a -> case reify p2 emptyF of
        This _ -> This a
        That _ -> That s
        These _ s'' -> These a s''
      That s' -> That s'
      These a s' -> case reify p2 s' of
        This _ -> This a
        That _ -> That s
        These _ s'' -> These a s''

instance (Uncons f s) => Monad (Parser f s) where
  Parser m >>= k = Parser $ Lift $ \s ->
    case reify m s of
      This a -> reify (let Parser p = k a in p) emptyF
      That s' -> That s'
      These a s' -> reify (let Parser p = k a in p) s'

-- | A parser that always fails (consumes nothing).
--
-- >>> runParser (empty :: Parser String Char Int) "abc"
-- That "abc"
empty :: Parser f s a
empty = Parser $ Lift $ \s -> That s

-- | Try the first parser. Left-biased.
--
-- >>> runParser (char 'a' <|> char 'b') "abc"
-- These 'a' "bc"
-- >>> runParser (char 'x' <|> char 'y') "abc"
-- That "abc"
(<|>) :: Parser f s a -> Parser f s a -> Parser f s a

infixl 3 <|>

(Parser p1) <|> (Parser p2) = Parser $ Knot (Lift body)
  where
    body (Right s) = case reify p1 s of
      This a -> Right (This a)
      That s' -> Left s'
      These a s' -> Right (These a s')
    body (Left s) = case reify p2 s of
      result -> Right result

-- | Skip zero or more elements matching the predicate.
--
-- >>> runParser (skipWhile (== ' ')) "   abc"
-- These () "abc"
skipWhile :: (Uncons f s) => (s -> Bool) -> Parser f s ()
skipWhile p = void (many (satisfy p))

-- | Capture the consumed portion of the input.
--
-- >>> runParser (captured (many (satisfy (/= ' ')))) "hello world"
-- These ("hello","hello") " world"
captured :: (HasLength f, HasEmpty f) => Parser f s a -> Parser f s (f, a)
captured p = Parser $ Lift $ \s ->
  case runParser p s of
    This a -> This (s, a)
    That _ -> That s
    These a s' -> These (streamTake (streamLength s - streamLength s') s, a) s'

-- | Zero or one repetition.
optional :: (Uncons f s) => Parser f s a -> Parser f s (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- | Skip zero or more repetitions.
skipMany :: (Uncons f s) => Parser f s a -> Parser f s ()
skipMany p = void (many p)

-- | Consume all remaining input.
--
-- >>> runParser takeRest "hello"
-- These "hello" ""
takeRest :: (HasEmpty f) => Parser f s f
takeRest = Parser $ Lift $ \f -> These f emptyF

-- | ASCII-only version of satisfy.
satisfyAscii :: (Uncons f Char) => (Char -> Bool) -> Parser f Char Char
satisfyAscii p = Parser $ Lift $ \f -> case uncons f of
  That _ -> That f
  These c f'
    | fromEnum c < 128, p c -> These c f'
    | otherwise -> That f
  This _ -> That f

-- | Try a parser with a fallback continuation.
withOption :: (Uncons f s) => Parser f s a -> (a -> Parser f s b) -> Parser f s b -> Parser f s b
withOption p f def = (p >>= f) <|> def

-- | Right-fold chain combinator.
chainr :: (Uncons f s) => (a -> b -> b) -> Parser f s a -> Parser f s b -> Parser f s b
chainr f p z = go
  where
    go = (f <$> p <*> go) <|> z

-- | Attempt a parser. If it fails with 'That', restore the original stream.
-- Useful for backtracking over committed consumption (e.g., trying inline-line
-- before bare-line).
--
-- >>> runParser (try (char 'a' >> char 'b')) "ac"
-- That "ac"
-- >>> runParser (try (char 'a' >> char 'b')) "ab"
-- These 'b' ""
try :: Parser f s a -> Parser f s a
try p = Parser $ Lift $ \s ->
  case runParser p s of
    That _ -> That s
    result -> result

-- | Parse exactly @n@ occurrences of the given parser.
--
-- >>> runParser (count 3 (char 'a')) "aaabc"
-- These "aaa" "bc"
-- >>> runParser (count 3 (char 'a')) "aabc"
-- That "aabc"
count :: (Uncons f s) => Int -> Parser f s a -> Parser f s [a]
count n p
  | n <= 0 = pure []
  | otherwise = (:) <$> p <*> count (n - 1) p

-- | Parse zero or more occurrences separated by a separator.
-- The separator is discarded.
--
-- >>> runParser (sepBy (char 'a') (char ',')) "a,a,a"
-- These "aaa" ""
-- >>> runParser (sepBy (char 'a') (char ',')) ""
-- These "" ""
sepBy :: (Uncons f s) => Parser f s a -> Parser f s b -> Parser f s [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | Parse one or more occurrences separated by a separator.
-- The separator is discarded. Uses committing '>>=' so separator consumption
-- is not backtracked.
--
-- >>> runParser (sepBy1 (char 'a') (char ',')) "a,a,a"
-- These "aaa" ""
-- >>> runParser (sepBy1 (char 'a') (char ',')) ""
-- That ""
sepBy1 :: (Uncons f s) => Parser f s a -> Parser f s b -> Parser f s [a]
sepBy1 p sep = p >>= \x -> many (try (sep >> p)) >>= \xs -> pure (x : xs)

-- | Succeed only at the end of input. Returns unit.
--
-- >>> runParser endOfInput ""
-- This ()
-- >>> runParser endOfInput "abc"
-- That "abc"
endOfInput :: (HasLength f, HasEmpty f) => Parser f s ()
endOfInput = Parser $ Lift $ \f ->
  if streamLength f == 0 then This () else That f

-- | Match a newline character or succeed at end of input.
-- Useful for line-oriented parsing where the last line
-- may not have a trailing newline.
--
-- >>> runParser lineEnd "\nabc"
-- These '\n' "abc"
-- >>> runParser lineEnd ""
-- These ' ' ""
lineEnd :: (Uncons f Char, HasLength f, HasEmpty f) => Parser f Char Char
lineEnd = char '\n' <|> (endOfInput Data.Functor.$> ' ')
