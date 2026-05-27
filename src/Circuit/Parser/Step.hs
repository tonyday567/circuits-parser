{-# LANGUAGE GHC2021 #-}

-- | Spike: three-outcome parser result that surfaces partial results to
-- combinators, without locking into a specific backtracking style.
--
--   'Yield' a f  — committed. consumed input, here's the result.
--   'Halt' f     — no progress. stream intact. try next alternative.
--   'Offer' a f  — partial. consumed to f, result handed to next alternative.
module Circuit.Parser.Step where

import Circuit (Circuit (..), reify)
import Control.Applicative (Alternative (..))
import Data.These (These (..))

-- | Three-outcome parse step.
data Step a f = Yield a f | Halt f | Offer a f
  deriving (Show, Eq)

-- | Parser wrapping a Circuit.
newtype Parser f s a = Parser { unParser :: Circuit (->) Either f (Step a f) }

-- | Run a parser (same pattern as Circuit.Parser.runParser).
run :: Parser f s a -> f -> Step a f
run = reify . unParser

-- | Commit: turn all offers into yields (attoparsec mode).
commit :: Parser f s a -> Parser f s a
commit (Parser p) = Parser $ Lift $ \s ->
  case reify p s of
    Offer a f -> Yield a f
    other     -> other

-- Builders

class Uncons f s where
  uncons :: f -> These s f

anyToken :: (Uncons f s) => Parser f s s
anyToken = Parser $ Lift $ \f -> case uncons f of
  That _ -> Halt f; These s f' -> Yield s f'; This _ -> Halt f

char :: (Uncons f s, Eq s) => s -> Parser f s s
char c = Parser $ Lift $ \f -> case uncons f of
  That _ -> Halt f
  These s f' | s == c -> Yield s f' | otherwise -> Halt f
  This _ -> Halt f

yield :: a -> Parser f s a
yield a = Parser $ Lift $ \f -> Yield a f

halt :: Parser f s a
halt = Parser $ Lift $ \f -> Halt f

offer :: a -> Parser f s a
offer a = Parser $ Lift $ \f -> Offer a f

-- Instances

instance Functor (Parser f s) where
  fmap f (Parser p) = Parser $ Lift $ \s ->
    case reify p s of
      Yield a s' -> Yield (f a) s'
      Halt s'    -> Halt s'
      Offer a s' -> Offer (f a) s'

instance Applicative (Parser f s) where
  pure = yield
  pf <*> px = Parser $ Lift $ \s ->
    case run pf s of
      Halt s'    -> Halt s'
      Yield f s' -> case run px s' of
        Halt _  -> Halt s
        Yield x s'' -> Yield (f x) s''
        Offer x s'' -> Offer (f x) s''
      Offer f s' -> case run px s' of
        Halt _  -> Halt s
        Yield x s'' -> Yield (f x) s''
        Offer x s'' -> Offer (f x) s''

instance Monad (Parser f s) where
  p >>= k = Parser $ Lift $ \s ->
    case run p s of
      Halt s'    -> Halt s'
      Yield a s' -> run (k a) s'
      Offer a s' -> run (k a) s'

instance Alternative (Parser f s) where
  empty = halt
  p1 <|> p2 = Parser $ Lift $ \s ->
    case run p1 s of
      Yield a s' -> Yield a s'
      Halt _     -> run p2 s
      Offer a s' -> run (feed a p2) s'

-- | Feed a partial result to a parser — p2 can accept or reject.
feed :: a -> Parser f s a -> Parser f s a
feed a p = Parser $ Lift $ \s ->
  case run p s of
    Halt _ -> Yield a s
    other  -> other

instance Uncons String Char where
  uncons [] = That []
  uncons (x : xs) = These x xs
