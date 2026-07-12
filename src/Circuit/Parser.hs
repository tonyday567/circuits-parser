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

    -- * These helpers
    asThese,
    asMaybe',
    asEither,

    -- * Choice
    empty,
    (<|>),

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

import Circuit (Trace (..), run, trace)
import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.ByteString qualified as B
import Data.Char (isAscii)
import Data.Functor (($>))
import Data.Functor qualified as F
import Data.Text (Text)
import Data.Text qualified as T
import Data.These (These (..), these)
import Data.Word (Word8)

-- | Typeclass for deconstructing input streams into head and tail.
--
-- The 'Monoid' superclass supplies the empty stream ('mempty'): after a
-- 'This' result the input is spent, but a continuation parser still needs
-- /some/ stream to run on, so the combinators feed it 'mempty'.
class (Monoid f) => Uncons f s where
  uncons :: f -> These s f

newtype Parser f s a = Parser
  { unParser :: Trace Either (->) f (These a f)
  }

-- | Run a parser on a stream, returning the raw 'These' result.
runParser :: Parser f s a -> f -> These a f
runParser = run . unParser

-- The final element is announced as 'This' by the 'Uncons' instance at the
-- point of extraction. 'pure' does not inspect the stream, so an exact
-- match leaves an empty remainder as @These a mempty@ rather than forcing
-- a final 'This' inside the parser. Each instance reuses the stream's own
-- decomposition and an O(1) emptiness test on the remainder (never a length
-- measurement — 'Text' length is O(n)).

instance Uncons [a] a where
  uncons [] = That []
  uncons [x] = This x
  uncons (x : xs) = These x xs

instance Uncons B.ByteString Char where
  uncons bs = case B.uncons bs of
    Nothing -> That bs
    Just (w, rest)
      | B.null rest -> This (w2c w)
      | otherwise -> These (w2c w) rest
    where
      w2c = toEnum . fromIntegral

instance Uncons B.ByteString Word8 where
  uncons bs = case B.uncons bs of
    Nothing -> That bs
    Just (w, rest)
      | B.null rest -> This w
      | otherwise -> These w rest

instance Uncons Text Char where
  uncons t = case T.uncons t of
    Nothing -> That t
    Just (c, rest)
      | T.null rest -> This c
      | otherwise -> These c rest

-- | Consume and return the next element, or 'That' if the stream is empty.
--
-- @
-- runParser anyToken \"abc\" = These 'a' \"bc\"
-- runParser anyToken \"c\"   = This 'c'
-- runParser anyToken \"\"    = That \"\"
-- @
anyToken :: (Uncons f s) => Parser f s s
anyToken = Parser $ Arr uncons

-- | Apply a predicate to the value inside a 'These' result. On failure the
-- original stream is returned intact.
guardThese :: (a -> Bool) -> f -> These a f -> These a f
guardThese p def =
  these
    (\a -> bool (That def) (This a) (p a))
    That
    (\a b -> bool (That def) (These a b) (p a))

-- | Consume one element if it satisfies the predicate.
--
-- >>> runParser (satisfy (> 'a')) "bcd"
-- These 'b' "cd"
-- >>> runParser (satisfy (> 'a')) "abc"
-- That "abc"
satisfy :: (Uncons f s) => (s -> Bool) -> Parser f s s
satisfy p = Parser $ Arr $ guardThese p <*> uncons

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
filterP (Parser p) f = Parser $ Arr $ guardThese f <*> run p

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
  fmap g (Parser p) = Parser (F.fmap (first g) p)

instance (Uncons f s) => Applicative (Parser f s) where
  pure a = Parser $ Arr $ These a

  Parser pf <*> pa = Parser $ Arr $ \s ->
    let app g s' = case runParser pa s' of
          That _ -> That s
          res -> first g res
     in case run pf s of
          That _ -> That s
          This g -> app g mempty
          These g s' -> app g s'

instance (Uncons f s) => Monad (Parser f s) where
  Parser m >>= k = Parser $ Arr $ \s ->
    case run m s of
      That s' -> That s'
      This a -> runParser (k a) mempty
      These a s' -> runParser (k a) s'

instance (Uncons f s) => Alternative (Parser f s) where
  empty = Parser $ Arr That
  Parser p1 <|> Parser p2 = Parser $ Arr $ trace $ \case
    Right s -> case run p1 s of
      That s' -> Left s'
      res -> Right res
    Left s -> Right (run p2 s)

-- | Skip zero or more elements matching the predicate.
--
-- >>> runParser (skipWhile (== ' ')) "   abc"
-- These () "abc"
skipWhile :: (Uncons f s) => (s -> Bool) -> Parser f s ()
skipWhile p = void (many (satisfy p))

-- | Zero or more repetitions. Accumulates results until failure.
--
-- Stops on 'That' (no progress), returning what was accumulated.
--
-- >>> runParser (many (char 'a')) "aaab"
-- These "aaa" "b"
-- >>> runParser (many (char 'a')) "xyz"
-- These "" "xyz"
many :: (Uncons f s) => Parser f s a -> Parser f s [a]
many p = some p <|> pure []

-- | One or more repetitions. Fails if the first parse fails.
--
-- >>> runParser (some (char 'a')) "aaab"
-- These "aaa" "b"
-- >>> runParser (some (char 'a')) "xyz"
-- That "xyz"
some :: (Uncons f s) => Parser f s a -> Parser f s [a]
some p = (:) <$> p <*> many p

-- | Zero or one repetition.
optional :: (Uncons f s) => Parser f s a -> Parser f s (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- | Skip zero or more repetitions.
skipMany :: (Uncons f s) => Parser f s a -> Parser f s ()
skipMany p = void (many p)

-- | Consume all remaining input. Spends the stream, so the result is 'This'.
--
-- >>> runParser takeRest "hello"
-- This "hello"
takeRest :: Parser f s f
takeRest = Parser $ Arr This

-- | Types that support O(n) or O(1) length and prefix taking.
class (Monoid f) => Slice f where
  sliceLength :: f -> Int
  sliceTake :: Int -> f -> f

instance Slice B.ByteString where
  sliceLength = B.length
  sliceTake = B.take

instance Slice Text where
  sliceLength = T.length
  sliceTake = T.take

instance Slice [a] where
  sliceLength = length
  sliceTake = take

-- | Capture the consumed portion of the input.
--
-- >>> runParser (captured (many (satisfy (/= ' ')))) "hello world"
-- These ("hello","hello") " world"
captured :: (Slice f) => Parser f s a -> Parser f s (f, a)
captured p = Parser $ Arr $ \s ->
  case runParser p s of
    This a -> This (s, a)
    That _ -> That s
    These a s' -> These (sliceTake (sliceLength s - sliceLength s') s, a) s'

-- | ASCII-only version of satisfy.
satisfyAscii :: (Uncons f Char) => (Char -> Bool) -> Parser f Char Char
satisfyAscii p = satisfy (\c -> isAscii c && p c)

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
-- This 'b'
try :: Parser f s a -> Parser f s a
try p = Parser $ Arr $ \s ->
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
-- End of input is exactly a 'That' from the 'uncons' coalgebra — no length
-- measurement, so this works for any 'Uncons' stream (including 'Text').
-- The element type @s@ is fixed by the surrounding parser; a standalone use
-- must pin it (as any @s@-polymorphic combinator does).
--
-- >>> runParser (endOfInput :: Parser String Char ()) ""
-- This ()
-- >>> runParser (endOfInput :: Parser String Char ()) "abc"
-- That "abc"
endOfInput :: forall f s. (Uncons f s) => Parser f s ()
endOfInput = Parser $ Arr $ \f -> case (uncons f :: These s f) of
  That _ -> This ()
  _ -> That f

-- | Match a newline character or succeed at end of input.
-- Useful for line-oriented parsing where the last line
-- may not have a trailing newline.
--
-- >>> runParser lineEnd "\nabc"
-- These '\n' "abc"
-- >>> runParser lineEnd ""
-- This ' '
lineEnd :: (Uncons f Char) => Parser f Char Char
lineEnd = char '\n' <|> (endOfInput $> ' ')
