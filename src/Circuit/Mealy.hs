{-# LANGUAGE CPP #-}

-- | Mealy machines as 'Circuit's with explicit state threading.
--
-- A 'Data.Mealy.Mealy' is a state machine @(inject, step, extract)@
-- where the state type is existentially hidden. By exposing the state
-- in the 'Circuit' tensor, we get:
--
--   * Composition where state threading is visible in types
--   * Access to 'Circuit''s 'Knot' for internal feedback
--   * Reuse of 'reify' and 'ambient' machinery
--
-- The key observation: a single step of a Mealy machine is a plain
-- function @(s, a) -> (s, b)@. A 'Circuit' over @(,)@ composes these
-- steps, and 'scanC' iterates the resulting circuit over a stream.
module Circuit.Mealy
  ( -- * Mealy as Circuit
    MealyC (..),
    fromStep,
    fromMealy,

    -- * Running
    scanC,
    foldC,

    -- * Strong combinators
    firstC,
    secondC,
    swapC,
    dupC,
  )
where

import Circuit
import Prelude hiding (id, (.))

#ifdef __GLASGOW_HASKELL__
import Control.Category
import Data.Bifunctor
import Data.Profunctor
#else
import Circuit.Classes
#endif

-- | A Mealy machine with explicit state @s@, expressed as a 'Circuit'
-- over the cartesian tensor.
--
-- @MealyC s a b@ threads state @s@ through a computation that maps
-- input @a@ to output @b@. Composition chains state through both
-- stages.
newtype MealyC s a b = MealyC {unMealyC :: Circuit (->) (,) (s, a) (s, b)}

-- | Lift a plain step function into a 'MealyC'.
--
-- @fromStep step extract@ creates a circuit where each input updates
-- the state and produces an output.
fromStep :: (s -> a -> s) -> (s -> b) -> MealyC s a b
fromStep step extract = MealyC $ Lift $ \(s, a) -> let s' = step s a in (s', extract s')

-- | Convert a 'Data.Mealy.Mealy' to a 'MealyC', returning the inject
-- function separately.
--
-- The resulting 'MealyC' expects the state to already be initialized;
-- use 'inject' to produce the initial state from the first input.
fromMealy :: (a -> s) -> (s -> a -> s) -> (s -> b) -> (a -> s, MealyC s a b)
fromMealy inject step extract = (inject, fromStep step extract)

-- | Run a 'MealyC' over a list, threading state manually.
--
-- >>> let m = fromStep (+) id :: MealyC Int Int Int
-- >>> scanC m 0 [1,2,3]
-- [1,3,6]
scanC :: MealyC s a b -> s -> [a] -> [b]
scanC (MealyC c) s0 as =
  let f = reify c
      go _ [] = []
      go s (a : as') = let (s', b) = f (s, a) in b : go s' as'
   in go s0 as

-- | Fold a 'MealyC' over a list, returning only the final output.
--
-- Throws an error on empty lists (mirrors 'Data.Mealy.fold').
foldC :: MealyC s a b -> s -> [a] -> b
foldC (MealyC c) s0 as =
  let f = reify c
   in case as of
        [] -> error "foldC: empty list"
        (a : as') -> snd $ foldl (\(s, _) a' -> f (s, a')) (f (s0, a)) as'

-- * Category instances

instance Category (MealyC s) where
  id = MealyC id
  MealyC f . MealyC g = MealyC (f . g)

instance Functor (MealyC s a) where
  fmap f (MealyC c) = MealyC (rmap (second f) c)

instance Profunctor (MealyC s) where
  dimap f g (MealyC c) = MealyC (dimap (second f) (second g) c)
  lmap f (MealyC c) = MealyC (lmap (second f) c)
  rmap g (MealyC c) = MealyC (rmap (second g) c)

-- * Strong combinators (state-threading variants)

-- | Apply a circuit to the first component of a pair, threading the
-- same state through both.
firstC :: MealyC s a b -> MealyC s (a, c) (b, c)
firstC (MealyC c) = MealyC $ Lift $ \(s, (a, x)) ->
  let (s', b) = reify c (s, a)
   in (s', (b, x))

-- | Apply a circuit to the second component of a pair.
secondC :: MealyC s a b -> MealyC s (c, a) (c, b)
secondC (MealyC c) = MealyC $ Lift $ \(s, (x, a)) ->
  let (s', b) = reify c (s, a)
   in (s', (x, b))

-- | Swap the two components of a pair (stateless).
swapC :: MealyC s (a, b) (b, a)
swapC = MealyC $ Lift $ \(s, (a, b)) -> (s, (b, a))

-- | Duplicate an input (stateless).
dupC :: MealyC s a (a, a)
dupC = MealyC $ Lift $ \(s, a) -> (s, (a, a))
