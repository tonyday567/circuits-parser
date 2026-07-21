{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A parser combinator library built on Circuit.
--
-- This module is the @Identity@ instantiation of
-- "Circuit.Parser.Unified": a pure, attoparsec-style parser over
-- @Loop Either (Kleisli Identity)@.
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
--   'That' f    — no progress, stream intact (for backtracking)
--   'These' a f — result + remainder (remainder may be empty)
--   'This' a    — optional boundary form only (extractors treat as success)
-- @
--
-- === doctests
--
-- >>> runParser (char 'a') "abc"
-- These 'a' "bc"
--
-- >>> runParser (char 'x') "abc"
-- That "abc"
--
-- >>> runParser (string "abc") "abc"
-- These "abc" ""
--
-- >>> runParser (string "abc") "abcd"
-- These "abc" "d"
--
-- >>> runParser (endOfInput :: Parser String Char ()) ""
-- These () ""
module Circuit.Parser
  ( -- * Type
    Parser,
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
    optional,
    skipMany,
    count,
    sepBy,
    sepBy1,
    chainr,

    -- * Capture (ByteString specialty)
    capturedBS,
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
    takeRest,
    satisfyAscii,
    withOption,
    try,
    endOfInput,
    lineEnd,
  )
where

import Circuit.Parser.Unified
  ( Uncons (..),
    anyToken,
    asEither,
    asMaybe',
    asThese,
    capturedBS,
    chainr,
    char,
    count,
    empty,
    endOfInput,
    filterP,
    lineEnd,
    many,
    optional,
    runParserIdentity,
    satisfy,
    satisfyAscii,
    sepBy,
    sepBy1,
    skipMany,
    skipWhile,
    some,
    string,
    takeRest,
    try,
    withOption,
    (<|>),
  )
import Circuit.Parser.Unified qualified as Unified
import Data.Functor.Identity (Identity)
import Data.These (These (..))
import Prelude hiding (id, (.))

-- | Parser over pure functions.
type Parser f s a = Unified.Parser Identity f s a

-- | Run a parser on a stream, returning the raw 'These' result.
runParser :: Parser f s a -> f -> These a f
runParser = runParserIdentity

-- | Run a parser and extract the result as Maybe.
runParserMaybe :: Parser f s a -> f -> Maybe a
runParserMaybe p f = asMaybe' (runParser p f)

-- | Run a parser and extract the result, erroring on failure.
runParserError :: Parser f s a -> f -> a
runParserError p f = asThese (runParser p f)
