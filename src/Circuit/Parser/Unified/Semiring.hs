{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Tiny semiring class and a weighted list monad for semiring parsing.
module Circuit.Parser.Unified.Semiring
  ( -- * Semiring classes
    Semiring (..),
    StarSemiring (..),

    -- * Carriers
    Count (..),
    Dual2 (..),
    Tropical (..),

    -- * Weighted source monad
    Weighted (..),
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Data.Kind (Type)
import Prelude

-- | A commutative semiring with additive identity 'zeroR' and multiplicative
-- identity 'oneR'.
class Semiring r where
  zeroR :: r
  oneR :: r
  plusR :: r -> r -> r
  timesR :: r -> r -> r

-- | Semirings with a closure operation @star x = 1 + x + x*x + ...@.
class (Semiring r) => StarSemiring r where
  star :: r -> r

instance Semiring Bool where
  zeroR = False
  oneR = True
  plusR = (||)
  timesR = (&&)

instance StarSemiring Bool where
  star _ = True

-- | Counting semiring: number of derivations.  It is /not/ a star semiring in
-- general because feedback cycles can yield infinitely many derivations; the
-- 'StarSemiring' instance below handles only the acyclic case.
newtype Count = Count Int
  deriving newtype (Eq, Ord, Num)
  deriving stock (Show)

instance Semiring Count where
  zeroR = Count 0
  oneR = Count 1
  plusR (Count x) (Count y) = Count (x + y)
  timesR (Count x) (Count y) = Count (x * y)

instance StarSemiring Count where
  star (Count 0) = Count 1
  star _ = error "Semiring.Count.star: infinite derivations"

-- | A dual number with two scalar parameters.  The first gradient component is
-- the partial derivative with respect to parameter A; the second with respect
-- to parameter B.  This is the semiring carrier that lets a weighted parser
-- return inside values and their parameter gradients simultaneously.
data Dual2 = Dual2
  { dualValue :: !Double,
    dualGradA :: !Double,
    dualGradB :: !Double
  }
  deriving (Eq, Show)

instance Semiring Dual2 where
  zeroR = Dual2 0 0 0
  oneR = Dual2 1 0 0
  plusR (Dual2 v1 ga1 gb1) (Dual2 v2 ga2 gb2) =
    Dual2 (v1 + v2) (ga1 + ga2) (gb1 + gb2)
  timesR (Dual2 v1 ga1 gb1) (Dual2 v2 ga2 gb2) =
    Dual2 (v1 * v2) (v1 * ga2 + v2 * ga1) (v1 * gb2 + v2 * gb1)

instance StarSemiring Dual2 where
  star (Dual2 0 0 0) = oneR
  star _ = error "Semiring.Dual2.star: productive cycle"

-- | Tropical semiring: additive identity is positive infinity, multiplicative
-- identity is zero, addition is minimum, and multiplication is addition.
--
-- This is the cost semiring for Viterbi / best-first parsing: the inside value
-- is the minimum cost of any derivation.
newtype Tropical = Tropical {unTropical :: Double}
  deriving newtype (Eq, Ord, Num, Show)

instance Semiring Tropical where
  zeroR = Tropical (1 / 0)
  oneR = Tropical 0
  plusR (Tropical x) (Tropical y) = Tropical (min x y)
  timesR (Tropical x) (Tropical y) = Tropical (x + y)

instance StarSemiring Tropical where
  star (Tropical x)
    | x >= 0 = oneR
    | otherwise = error "Semiring.Tropical.star: negative cycle"

-- | A list monad that carries a semiring weight with each result.  'return'
-- attaches 'oneR'; 'bind' multiplies weights along a derivation; 'mplus'
-- collects alternatives without merging them.  The target embedding sums
-- weights when the same output is reached by multiple derivations.
newtype Weighted (r :: Type) a = Weighted {runWeighted :: [(a, r)]}
  deriving stock (Eq, Show, Functor)

instance (Semiring r) => Applicative (Weighted r) where
  pure a = Weighted [(a, oneR)]
  Weighted fs <*> Weighted xs =
    Weighted [(f x, timesR wf wx) | (f, wf) <- fs, (x, wx) <- xs]

instance (Semiring r) => Monad (Weighted r) where
  Weighted xs >>= f =
    Weighted [(b, timesR w1 w2) | (a, w1) <- xs, (b, w2) <- runWeighted (f a)]

instance (Semiring r) => Alternative (Weighted r) where
  empty = Weighted []
  Weighted xs <|> Weighted ys = Weighted (xs ++ ys)

instance (Semiring r) => MonadPlus (Weighted r) where
  mzero = Weighted []
  mplus (Weighted xs) (Weighted ys) = Weighted (xs ++ ys)
