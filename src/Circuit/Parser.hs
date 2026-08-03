{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Unified parser syntax over @Loop Either (Kleisli m)@.
--
-- This module provides the free traced-monoidal parser skeleton. The base
-- monad @m@ selects the parser family:
--
--   * @m = Identity@ — attoparsec-style pure parser
--   * @m = StateT s (ExceptT e n)@ — megaparsec-style state + errors
--   * @m = LogicT n@ — LogicT-style nondeterministic branching
--
-- First-line libraries add 'Applicative', 'Alternative', 'Monad', and
-- 'MonadLogic' instances on top of this syntax.
--
-- === the intact-stream law
--
-- Every parser that fails returns 'That' carrying the /intact original/
-- stream. This is the invariant that makes '<|>' backtrack: the next
-- alternative receives the same stream the previous one started with. If a
-- composite parser consumes input before failing, its 'That' carries the
-- stream /at the point of failure/, so the next alternative will silently
-- start from a partially consumed position. 'try' repairs exactly that:
-- wrap a composite alternative when it may consume input and then fail.
--
-- === doctests
--
-- >>> runParserIdentity (char 'a') "abc"
-- These 'a' "bc"
--
-- >>> runParserIdentity (char 'x') "abc"
-- That "abc"
--
-- >>> runParserIdentity (string "ab") "abc"
-- These "ab" "c"
--
-- >>> runParserIdentity (string "ab") "ab"
-- These "ab" ""
--
-- >>> runParserIdentity (many (char 'a')) "aaab"
-- These "aaa" "b"
--
-- >>> runParserIdentity (char 'a' *> char 'b') "ab"
-- This 'b'
--
-- >>> runParserIdentity (char 'a' *> char 'b') "a"
-- That "a"
--
-- >>> runParserIdentity (endOfInput :: Parser Identity String Char ()) ""
-- These () ""
--
-- >>> runParserIdentity (endOfInput :: Parser Identity String Char ()) "a"
-- That "a"
module Circuit.Parser
  ( -- * Result type
    These (..),

    -- * Stream coalgebra
    Uncons (..),

    -- * Parser syntax
    Parser (..),

    -- * Running
    runParser,
    runParserIdentity,
    runParserMaybe,
    runParserError,

    -- * Result extraction
    asThese,
    asMaybe',
    asEither,

    -- * Primitives
    next,
    anyToken,
    satisfy,
    satisfyAscii,
    char,
    string,
    endOfInput,
    takeRest,
    skipWhile,

    -- * Choice
    empty,
    (<|>),

    -- * Repetition
    many,
    some,
    optional,
    skipMany,
    count,
    sepBy,
    sepBy1,
    chainr,

    -- * Capture
    capturedBS,
    bs,
    span,
    span1,

    -- * Inspection
    peek,

    -- * Backtracking
    try,

    -- * Post-filter
    filterP,

    -- * Continuation
    withOption,

    -- * Line endings
    lineEnd,
  )
where

import Circuit (Loop (..), run, trace)
import Circuit.Category (Category (..), (.>))
import Circuit.Channel (Traced)
import Circuit.Stream (These (..), Uncons (..))
import Control.Applicative (Alternative (empty, (<|>)))
import Control.Arrow (Kleisli (..))
import Control.Monad (MonadPlus, void)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Char (isAscii)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.These (these)
import Prelude hiding (id, span, (.))

-- $setup
-- >>> import Data.Functor.Identity (Identity)
-- >>> import Data.These (These (..))

-- | Parser syntax: a @Loop Either@ morphism in @Kleisli m@ from stream @f@ to
-- result @These a f@.
newtype Parser m f s a = Parser
  { unParser :: Loop Either (Kleisli m) f (These a f)
  }

-- | Run a parser in the base monad, returning the raw 'These' result.
runParser :: forall m f s a. (Monad m, Traced Either (Kleisli m)) => Parser m f s a -> f -> m (These a f)
runParser p = runKleisli (run (unParser p))

-- | Run a pure parser.
runParserIdentity :: Parser Identity f s a -> f -> These a f
runParserIdentity p = runIdentity . runParser p

-- | Run a parser and convert the result to 'Maybe'.
runParserMaybe :: forall m f s a. (Monad m, Traced Either (Kleisli m)) => Parser m f s a -> f -> m (Maybe a)
runParserMaybe p = fmap asMaybe' . runParser p

-- | Run a parser and extract the result, erroring on failure.
runParserError :: forall m f s a. (Monad m, Traced Either (Kleisli m)) => Parser m f s a -> f -> m a
runParserError p = fmap asThese . runParser p

-- | Extract value from a parse result, erroring on failure.
asThese :: These a f -> a
asThese (This a) = a
asThese (These a _) = a
asThese (That _) = error "parse failed"

-- | Convert a parse result to 'Maybe'.
asMaybe' :: These a f -> Maybe a
asMaybe' (This a) = Just a
asMaybe' (These a _) = Just a
asMaybe' (That _) = Nothing

-- | Convert a parse result to 'Either' (failure returns 'Left' with the leftover stream).
asEither :: These a f -> Either f a
asEither (This a) = Right a
asEither (These a _) = Right a
asEither (That f) = Left f

-- | Consume and return the next element, or 'That' if the stream is empty.
next :: forall m f s. (Monad m, Uncons f s) => Parser m f s s
next = Parser $ Lift $ Kleisli $ \f -> pure (uncons f)

-- | Alias for 'next'.
anyToken :: forall m f s. (Monad m, Uncons f s) => Parser m f s s
anyToken = next

-- | Apply a predicate to the result of 'uncons'.
guardThese :: (Uncons f s) => (s -> Bool) -> f -> These s f -> These s f
guardThese p def =
  these
    (\a -> bool (That def) (This a) (p a))
    That
    (\a b -> bool (That def) (These a b) (p a))

-- | Consume one element if it satisfies the predicate.
satisfy :: forall m f s. (Monad m, Uncons f s) => (s -> Bool) -> Parser m f s s
satisfy p = Parser $ Lift $ Kleisli $ \f -> pure (guardThese p f (uncons f))

-- | Apply a predicate to the value inside a 'These' result. On failure the
-- original stream is returned intact.
guardResult :: (a -> Bool) -> f -> These a f -> These a f
guardResult p def =
  these
    (\a -> bool (That def) (This a) (p a))
    That
    (\a b -> bool (That def) (These a b) (p a))

-- | Keep only successes matching the predicate.
filterP :: forall m f s a. (Monad m, Uncons f s, Traced Either (Kleisli m)) => Parser m f s a -> (a -> Bool) -> Parser m f s a
filterP (Parser p) f = Parser $ Lift $ Kleisli $ \s -> fmap (guardResult f s) (runKleisli (run p) s)

-- | Match a specific element.
char :: forall m f s. (Monad m, Uncons f s, Eq s) => s -> Parser m f s s
char c = satisfy (== c)

-- | Match a sequence of elements.
string :: forall m f s. (Monad m, Uncons f s, Eq s) => [s] -> Parser m f s [s]
string = traverse char

-- | Succeed only at the end of input.
endOfInput :: forall m f s. (Monad m, Uncons f s) => Parser m f s ()
endOfInput = Parser $ Lift $ Kleisli $ \f ->
  pure $ case uncons @f @s f of
    That _ -> These () f
    _ -> That f

instance (Monad m) => Functor (Parser m f s) where
  fmap g (Parser p) = Parser (p .> Lift (Kleisli (pure . first g)))

instance (Monad m, Uncons f s) => Applicative (Parser m f s) where
  pure a = Parser $ Lift $ Kleisli $ \f -> pure (These a f)

  Parser pf <*> Parser pa = Parser $ Lift $ Kleisli $ \s -> do
    let app g s' =
          runKleisli (run pa) s' >>= \case
            That _ -> pure (That s)
            res -> pure (first g res)
    runKleisli (run pf) s >>= \case
      That _ -> pure (That s)
      This g -> app g (nil @f @s)
      These g s' -> app g s'

instance (Monad m, Uncons f s) => Monad (Parser m f s) where
  Parser m >>= k = Parser $ Lift $ Kleisli $ \s -> do
    runKleisli (run m) s >>= \case
      That s' -> pure (That s')
      This a -> runKleisli (run (unParser (k a))) (nil @f @s)
      These a s' -> runKleisli (run (unParser (k a))) s'

instance (Monad m, Uncons f s, Traced Either (Kleisli m)) => Alternative (Parser m f s) where
  empty = Parser $ Lift $ Kleisli $ \f -> pure (That f)

  Parser p1 <|> Parser p2 = Parser $ trace $ Lift $ Kleisli $ \case
    Right s -> do
      res <- runKleisli (run p1) s
      case res of
        That s' -> pure (Left s')
        _ -> pure (Right res)
    Left s -> do
      res <- runKleisli (run p2) s
      pure (Right res)

instance (Monad m, Uncons f s, Traced Either (Kleisli m)) => MonadPlus (Parser m f s)

-- | Zero or more repetitions.
many :: forall m f s a. (Monad m, Uncons f s, Traced Either (Kleisli m)) => Parser m f s a -> Parser m f s [a]
many p = some p <|> pure []

-- | One or more repetitions.
some :: forall m f s a. (Monad m, Uncons f s, Traced Either (Kleisli m)) => Parser m f s a -> Parser m f s [a]
some p = (:) <$> p <*> many p

-- | ASCII-only version of satisfy.
satisfyAscii :: forall m f. (Monad m, Uncons f Char) => (Char -> Bool) -> Parser m f Char Char
satisfyAscii p = satisfy (\c -> isAscii c && p c)

-- | Skip zero or more elements matching the predicate.
skipWhile :: forall m f s. (Monad m, Uncons f s, Traced Either (Kleisli m)) => (s -> Bool) -> Parser m f s ()
skipWhile p = void (many (satisfy p))

-- | Consume all remaining input as the value.
takeRest :: forall m f s. (Monad m, Uncons f s) => Parser m f s f
takeRest = Parser $ Lift $ Kleisli $ \s -> pure (These s (nil @f @s))

-- | Zero or one repetition.
optional :: forall m f s a. (Monad m, Uncons f s, Traced Either (Kleisli m)) => Parser m f s a -> Parser m f s (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- | Skip zero or more repetitions.
skipMany :: forall m f s a. (Monad m, Uncons f s, Traced Either (Kleisli m)) => Parser m f s a -> Parser m f s ()
skipMany p = void (many p)

-- | Parse exactly @n@ occurrences of the given parser.
count :: forall m f s a. (Monad m, Uncons f s, Traced Either (Kleisli m)) => Int -> Parser m f s a -> Parser m f s [a]
count n p
  | n <= 0 = pure []
  | otherwise = (:) <$> p <*> count (n - 1) p

-- | Parse zero or more occurrences separated by a separator.
-- The separator is discarded.
sepBy :: forall m f s a b. (Monad m, Uncons f s, Traced Either (Kleisli m)) => Parser m f s a -> Parser m f s b -> Parser m f s [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | Parse one or more occurrences separated by a separator.
-- The separator is discarded. /Trailing separators are rejected/: after a
-- separator, the element parser must succeed. Use 'try' on the separator
-- yourself only if you genuinely want to allow trailing separators.
sepBy1 :: forall m f s a b. (Monad m, Uncons f s, Traced Either (Kleisli m)) => Parser m f s a -> Parser m f s b -> Parser m f s [a]
sepBy1 p sep = p >>= \x -> many (sep >> p) >>= \xs -> pure (x : xs)

-- | Right-fold chain combinator.
chainr :: forall m f s a b. (Monad m, Uncons f s, Traced Either (Kleisli m)) => (a -> b -> b) -> Parser m f s a -> Parser m f s b -> Parser m f s b
chainr f p z = go
  where
    go = (f <$> p <*> go) <|> z

-- | Attempt a parser. If it fails with 'That', restore the original stream.
--
-- This matters for composite alternatives consumed by '<|>': a parser that
-- consumes input before failing would otherwise hand the next alternative a
-- partially-consumed stream. Wrap the composite in 'try' when that is
-- possible.
try :: forall m f s a. (Monad m, Uncons f s, Traced Either (Kleisli m)) => Parser m f s a -> Parser m f s a
try (Parser p) = Parser $ Lift $ Kleisli $ \s -> do
  res <- runKleisli (run p) s
  pure $ case res of
    That _ -> That s
    result -> result

-- | Try a parser with a fallback continuation.
withOption :: forall m f s a b. (Monad m, Uncons f s, Traced Either (Kleisli m)) => Parser m f s a -> (a -> Parser m f s b) -> Parser m f s b -> Parser m f s b
withOption p f def = (p >>= f) <|> def

-- | Match a newline character or succeed at end of input.
lineEnd :: forall m f. (Monad m, Uncons f Char, Traced Either (Kleisli m)) => Parser m f Char Char
lineEnd = char '\n' <|> (endOfInput $> ' ')

-- | Capture the matched 'ByteString' prefix of a successful parse.
--
-- Flatparse-era specialty: measure consumed length via @B.length@ on the
-- remainder and 'B.take' a prefix of the original (cheap for strict
-- 'ByteString').
capturedBS :: forall m a. (Monad m, Traced Either (Kleisli m)) => Parser m ByteString Char a -> Parser m ByteString Char (ByteString, a)
capturedBS (Parser p) = Parser $ Lift $ Kleisli $ \s -> do
  res <- runKleisli (run p) s
  pure $ case res of
    That _ -> That s
    This a -> These (s, a) B.empty
    These a s' -> These (B.take (B.length s - B.length s') s, a) s'

-- | Match a span and return it as a 'ByteString'.
bs :: forall m a. (Monad m, Traced Either (Kleisli m)) => Parser m ByteString Char a -> Parser m ByteString Char ByteString
bs p = fst <$> capturedBS p

-- | Capture a (possibly empty) span of elements satisfying the predicate.
-- The result is the list of captured elements; for zero-copy capture of a
-- 'ByteString' span, prefer 'bs' with 'skipWhile'.
span :: forall m f s. (Monad m, Uncons f s) => (s -> Bool) -> Parser m f s [s]
span p = Parser $ Lift $ Kleisli $ \s -> pure $ go [] s
  where
    go acc s0 = case uncons @f @s s0 of
      That _ -> These (reverse acc) s0
      This x
        | p x -> These (reverse (x : acc)) (nil @f @s)
        | otherwise -> These (reverse acc) s0
      These x s'
        | p x -> go (x : acc) s'
        | otherwise -> These (reverse acc) s0

-- | Capture a non-empty span of elements satisfying the predicate. Fails if
-- the next element does not satisfy the predicate.
span1 :: forall m f s. (Monad m, Uncons f s) => (s -> Bool) -> Parser m f s [s]
span1 p = Parser $ Lift $ Kleisli $ \s -> pure $
  case uncons @f @s s of
    That _ -> That s
    This x
      | p x -> These [x] (nil @f @s)
      | otherwise -> That s
    These x s'
      | p x -> go [x] s'
      | otherwise -> That s
  where
    go acc s0 = case uncons @f @s s0 of
      That _ -> These (reverse acc) s0
      This x
        | p x -> These (reverse (x : acc)) (nil @f @s)
        | otherwise -> These (reverse acc) s0
      These x s'
        | p x -> go (x : acc) s'
        | otherwise -> These (reverse acc) s0

-- | Return the next element without consuming the stream. Fails at end of
-- input.
peek :: forall m f s. (Monad m, Uncons f s) => Parser m f s s
peek = Parser $ Lift $ Kleisli $ \s -> pure $ case uncons @f @s s of
  That _ -> That s
  This x -> These x s
  These x _ -> These x s
