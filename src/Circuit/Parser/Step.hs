{-# LANGUAGE GHC2021 #-}

-- | Spike: three-outcome parser result that surfaces partial results to
-- combinators, without locking into a specific backtracking style.
--
--   'Yield' a f  — committed. consumed input, here's the result.
--   'Halt' f     — no progress. stream intact. try next alternative.
--   'Offer' a f  — partial. consumed to f, result handed to next alternative.
module Circuit.Parser.Step where

import Circuit (Trace (..), run)
import Control.Applicative (Alternative (..))
import Data.These (These (..))

-- | Three-outcome parse step.
data Step a f = Yield a f | Halt f | Offer a f
  deriving (Show, Eq)

-- | Parser wrapping a Circuit.
newtype Parser f s a = Parser {unParser :: Trace Either (->) f (Step a f)}

-- | Run a parser (same pattern as Circuit.Parser.runParser).
runStep :: Parser f s a -> f -> Step a f
runStep = run . unParser

-- | Commit: turn all offers into yields (attoparsec mode).
commit :: Parser f s a -> Parser f s a
commit (Parser p) = Parser $ Arr $ \s ->
  case run p s of
    Offer a f -> Yield a f
    other -> other

-- Builders

class Uncons f s where
  uncons :: f -> These s f

anyToken :: (Uncons f s) => Parser f s s
anyToken = Parser $ Arr $ \f -> case uncons f of
  That _ -> Halt f
  These s f' -> Yield s f'
  This _ -> Halt f

char :: (Uncons f s, Eq s) => s -> Parser f s s
char c = Parser $ Arr $ \f -> case uncons f of
  That _ -> Halt f
  These s f' | s == c -> Yield s f' | otherwise -> Halt f
  This _ -> Halt f

yield :: a -> Parser f s a
yield a = Parser $ Arr $ \f -> Yield a f

halt :: Parser f s a
halt = Parser $ Arr $ \f -> Halt f

offer :: a -> Parser f s a
offer a = Parser $ Arr $ \f -> Offer a f

-- Instances

instance Functor (Parser f s) where
  fmap f (Parser p) = Parser $ Arr $ \s ->
    case run p s of
      Yield a s' -> Yield (f a) s'
      Halt s' -> Halt s'
      Offer a s' -> Offer (f a) s'

instance Applicative (Parser f s) where
  pure = yield
  pf <*> px = Parser $ Arr $ \s ->
    case runStep pf s of
      Halt s' -> Halt s'
      Yield f s' -> case runStep px s' of
        Halt _ -> Halt s
        Yield x s'' -> Yield (f x) s''
        Offer x s'' -> Offer (f x) s''
      Offer f s' -> case runStep px s' of
        Halt _ -> Halt s
        Yield x s'' -> Yield (f x) s''
        Offer x s'' -> Offer (f x) s''

instance Monad (Parser f s) where
  p >>= k = Parser $ Arr $ \s ->
    case runStep p s of
      Halt s' -> Halt s'
      Yield a s' -> runStep (k a) s'
      Offer a s' -> runStep (k a) s'

instance Alternative (Parser f s) where
  empty = halt
  p1 <|> p2 = Parser $ Arr $ \s ->
    case runStep p1 s of
      Yield a s' -> Yield a s'
      Halt _ -> runStep p2 s
      Offer a s' -> runStep (feed a p2) s'

-- | Feed a partial result to a parser — p2 can accept or reject.
feed :: a -> Parser f s a -> Parser f s a
feed a p = Parser $ Arr $ \s ->
  case runStep p s of
    Halt _ -> Yield a s
    other -> other

instance Uncons String Char where
  uncons [] = That []
  uncons (x : xs) = These x xs
