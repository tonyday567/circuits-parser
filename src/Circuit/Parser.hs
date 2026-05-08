{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
  )
where

import Circuit (Circuit (..), reify)
import Data.These (These (..))
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T

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
class HasEmpty f => Uncons f s where
  uncons :: f -> These s f

newtype Parser f s a = Parser
  { unParser :: Circuit (->) Either f (These a f)
  }

-- | Run a parser on a stream, returning the 'These' result.
runParser :: Parser f s a -> f -> These a f
runParser = reify . unParser

instance Uncons [a] a where
  uncons []     = That []
  uncons (x:xs) = These x xs

instance Uncons B.ByteString Char where
  uncons bs
    | B.null bs = That bs
    | otherwise = These (toEnum (fromIntegral (B.head bs)) :: Char) (B.tail bs)

instance Uncons Text Char where
  uncons t
    | T.null t = That t
    | otherwise = These (T.head t) (T.tail t)

-- | Consume and return the next element, or 'That' if the stream is empty.
--
-- >>> runParser anyToken "abc"
-- These 'a' "bc"
-- >>> runParser anyToken ""
-- That ""
anyToken :: Uncons f s => Parser f s s
anyToken = Parser $ Lift $ \f -> case uncons f of
  That _    -> That f
  These s f' -> These s f'

-- | Consume one element if it satisfies the predicate.
--
-- >>> runParser (satisfy (> 'a')) "bcd"
-- These 'b' "cd"
-- >>> runParser (satisfy (> 'a')) "abc"
-- That "abc"
satisfy :: Uncons f s => (s -> Bool) -> Parser f s s
satisfy p = Parser $ Lift $ \f -> case uncons f of
  That _         -> That f
  These s f' | p s -> These s f'
             | otherwise -> That f

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
filterP :: Uncons f s => Parser f s a -> (a -> Bool) -> Parser f s a
filterP (Parser p) f = Parser $ Lift $ \s ->
  case reify p s of
    This a    | f a -> This a
              | otherwise -> That s
    That s'          -> That s'
    These a s' | f a -> These a s'
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
      This a     -> This (reverse (a : acc))
      These a s' -> go s' (a : acc)
      That s'    -> These (reverse acc) s'

-- | One or more repetitions. Fails if the first parse fails.
--
-- >>> runParser (some (char 'a')) "aaab"
-- These "aaa" "b"
-- >>> runParser (some (char 'a')) "xyz"
-- That "xyz"
some :: Uncons f s => Parser f s a -> Parser f s [a]
some p = (:) <$> p <*> many p

-- | Thread state through 'These' results.
thenThese :: HasEmpty f => These a f -> (a -> f -> These b f) -> These b f
thenThese (This a)    f = f a emptyF
thenThese (That s)    _ = That s
thenThese (These a s) f = f a s

-- | Extract value from These.
asThese :: Show e => These e a -> a
asThese (That a)    = a
asThese (This e)    = error (show e)
asThese (These _ a) = a

-- | Convert These to Maybe, strictly.
asMaybe' :: These e a -> Maybe a
asMaybe' (That a)    = Just a
asMaybe' (This _)    = Nothing
asMaybe' (These _ _) = Nothing

instance Functor (Parser f s) where
  fmap f (Parser p) = Parser $ Lift $ \s ->
    case reify p s of
      This a     -> This (f a)
      That s'    -> That s'
      These a s' -> These (f a) s'

instance Uncons f s => Applicative (Parser f s) where
  pure a = Parser $ Lift $ \f -> These a f
  Parser pf <*> Parser pa = Parser $ Lift $ \s ->
    case reify pf s of
      This f     -> This f `thenThese` (\_ s' -> case reify pa s' of
                      This a'    -> This (f a')
                      That _     -> That s
                      These a' s''' -> These (f a') s''')
      That s'    -> That s'
      These f s' -> These f s' `thenThese` (\_ s'' -> case reify pa s'' of
                      This a'    -> This (f a')
                      That _     -> That s
                      These a' s''' -> These (f a') s''')
  Parser p1 *> Parser p2 = Parser $ Lift $ \s ->
    case reify p1 s of
      This _     -> reify p2 emptyF
      That s'    -> That s'
      These _ s' -> case reify p2 s' of
                      That _  -> That s
                      result  -> result
  Parser p1 <* Parser p2 = Parser $ Lift $ \s ->
    case reify p1 s of
      This a     -> case reify p2 emptyF of
                      This _      -> This a
                      That _      -> That s
                      These _ s'' -> These a s''
      That s'    -> That s'
      These a s' -> case reify p2 s' of
                      This _      -> This a
                      That _      -> That s
                      These _ s'' -> These a s''

instance Uncons f s => Monad (Parser f s) where
  Parser m >>= k = Parser $ Lift $ \s ->
    case reify m s of
      This a     -> reify (let Parser p = k a in p) emptyF
      That s'    -> That s'
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
(Parser p1) <|> (Parser p2) = Parser $ Loop body
  where
    body (Right s) = case reify p1 s of
      This a     -> Right (This a)
      That s'    -> Left s'
      These a s' -> Right (These a s')
    body (Left s) = case reify p2 s of
      result -> Right result

-- | Skip zero or more elements matching the predicate.
--
-- >>> runParser (skipWhile (== ' ')) "   abc"
-- These () "abc"
skipWhile :: Uncons f s => (s -> Bool) -> Parser f s ()
skipWhile p = () <$ many (satisfy p)

-- | Capture the consumed portion of the input.
--
-- >>> runParser (captured (many (satisfy (/= ' ')))) "hello world"
-- These ("hello", "hello") " world"
captured :: (HasLength f, HasEmpty f) => Parser f s a -> Parser f s (f, a)
captured p = Parser $ Lift $ \s ->
  case runParser p s of
    This a     -> This (s, a)
    That _     -> That s
    These a s' -> These (streamTake (streamLength s - streamLength s') s, a) s'

-- | Zero or one repetition.
optional :: Uncons f s => Parser f s a -> Parser f s (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- | Skip zero or more repetitions.
skipMany :: Uncons f s => Parser f s a -> Parser f s ()
skipMany p = () <$ many p

-- | Consume all remaining input.
--
-- >>> runParser takeRest "hello"
-- These "hello" ""
takeRest :: HasEmpty f => Parser f s f
takeRest = Parser $ Lift $ \f -> These f emptyF

-- | ASCII-only version of satisfy.
satisfyAscii :: Uncons f Char => (Char -> Bool) -> Parser f Char Char
satisfyAscii p = Parser $ Lift $ \f -> case uncons f of
  That _         -> That f
  These c f' | fromEnum c < 128, p c -> These c f'
             | otherwise              -> That f

-- | Try a parser with a fallback continuation.
withOption :: Uncons f s => Parser f s a -> (a -> Parser f s b) -> Parser f s b -> Parser f s b
withOption p f def = (p >>= f) <|> def

-- | Right-fold chain combinator.
chainr :: Uncons f s => (a -> b -> b) -> Parser f s a -> Parser f s b -> Parser f s b
chainr f p z = go
  where
    go = (f <$> p <*> go) <|> z
