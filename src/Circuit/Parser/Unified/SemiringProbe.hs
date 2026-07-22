{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Semiring-weighted interpretation of 'Loop Either' parsers.
--
-- This is the next step after the branching-trace probe: instead of listing
-- parse trees, we sum their weights.  A parser is interpreted as a weighted
-- relation @input -> [(output, weight)]@, and the 'Either' trace computes the
-- total weight of all paths through a choice loop.
--
-- The source parser is built in the list monad so that the choice combinator
-- can emit /both/ a success edge and a failure-feedback edge.  The target
-- category then sums over those edges.  This separates the "branching
-- structure" (list monad) from the "semiring accumulation" ('SemiringKleisli').
--
-- For now the weighted trace is acyclic: it diverges on genuine feedback
-- cycles such as 'many'.  That is fine for the first experiment; handling
-- cyclic repetition is the following step.
module Circuit.Parser.Unified.SemiringProbe
  ( Semiring (..),
    Count (..),
    SemiringKleisli (..),
    altW,
    runSemiringParser,
    totalWeight,
    demo,
  )
where

import Circuit qualified as C
import Circuit.Category (Category (..), Discrete (..))
import Circuit.Channel (Channel (..), Strength (..), Traced (..))
import Circuit.Parser.Unified
  ( Parser (..),
    Uncons,
    string,
  )
import Circuit.Tensor (Action (..), Tensor (..))
import Control.Arrow (Kleisli (..), runKleisli)
import Data.These (These (..))
import Prelude hiding (id, (.))

-- ---------------------------------------------------------------------------
-- Tiny semiring class
-- ---------------------------------------------------------------------------

-- | A commutative semiring with additive identity 'zeroR' and multiplicative
-- identity 'oneR'.
class Semiring r where
  zeroR :: r
  oneR :: r
  plusR :: r -> r -> r
  timesR :: r -> r -> r

instance Semiring Bool where
  zeroR = False
  oneR = True
  plusR = (||)
  timesR = (&&)

-- | Counting semiring: number of derivations.
newtype Count = Count Int
  deriving (Eq, Ord, Show, Num)

instance Semiring Count where
  zeroR = Count 0
  oneR = Count 1
  plusR (Count x) (Count y) = Count (x + y)
  timesR (Count x) (Count y) = Count (x * y)

-- ---------------------------------------------------------------------------
-- Weighted relation arrow
-- ---------------------------------------------------------------------------

-- | A morphism @a -> b@ is a weighted relation: for each input it produces a
-- finite bag of outputs together with their weights.
newtype SemiringKleisli r a b = SemiringKleisli
  { runSemiringKleisli :: a -> [(b, r)]
  }

instance (Semiring r) => Category (SemiringKleisli r) where
  type Ob (SemiringKleisli r) a = ()
  id = SemiringKleisli $ \a -> [(a, oneR)]
  SemiringKleisli g . SemiringKleisli f =
    SemiringKleisli $ \a ->
      [ (c, timesR w1 w2)
        | (b, w1) <- f a,
          (c, w2) <- g b
      ]

instance (Semiring r) => Discrete (SemiringKleisli r) where
  withOb x = x

instance (Semiring r) => Channel (,) (SemiringKleisli r) where
  assoc = SemiringKleisli $ \((a, b), c) -> [((a, (b, c)), oneR)]
  assoc' = SemiringKleisli $ \(a, (b, c)) -> [(((a, b), c), oneR)]
  slide = SemiringKleisli $ \(a, (b, c)) -> [((b, (a, c)), oneR)]

instance (Semiring r) => Channel Either (SemiringKleisli r) where
  assoc = SemiringKleisli $ \case
    Left (Left a) -> [(Left a, oneR)]
    Left (Right b) -> [(Right (Left b), oneR)]
    Right c -> [(Right (Right c), oneR)]
  assoc' = SemiringKleisli $ \case
    Left a -> [(Left (Left a), oneR)]
    Right (Left b) -> [(Left (Right b), oneR)]
    Right (Right c) -> [(Right c, oneR)]
  slide = SemiringKleisli $ \case
    Left a -> [(Right (Left a), oneR)]
    Right (Left b) -> [(Left b, oneR)]
    Right (Right c) -> [(Right (Right c), oneR)]

instance (Semiring r) => Strength (,) (SemiringKleisli r) where
  strength f = SemiringKleisli $ \(a, b) ->
    [ ((a, c), w) | (c, w) <- runSemiringKleisli f b
    ]

instance (Semiring r) => Strength Either (SemiringKleisli r) where
  strength f = SemiringKleisli $ \case
    Left a -> [(Left a, oneR)]
    Right b ->
      [ (Right c, w) | (c, w) <- runSemiringKleisli f b
      ]

instance (Semiring r) => Tensor (,) (SemiringKleisli r) where
  par f g = SemiringKleisli $ \(a, c) ->
    [ ((b, d), timesR w1 w2)
      | (b, w1) <- runSemiringKleisli f a,
        (d, w2) <- runSemiringKleisli g c
    ]
  unitl = SemiringKleisli $ \((), a) -> [(a, oneR)]
  unitl' = SemiringKleisli $ \a -> [(((), a), oneR)]
  unitr = SemiringKleisli $ \(a, ()) -> [(a, oneR)]
  unitr' = SemiringKleisli $ \a -> [((a, ()), oneR)]

instance (Semiring r) => Action (,) (SemiringKleisli r) where
  swap = SemiringKleisli $ \(a, b) -> [((b, a), oneR)]

-- | Sum over all 'Right' exits reachable from the initial 'Right' input.
--
-- This implementation is recursive and assumes the feedback graph is acyclic;
-- cycles (e.g. from 'many') will diverge.
instance (Semiring r) => Traced Either (SemiringKleisli r) where
  trace (SemiringKleisli f) = SemiringKleisli $ go . Right
    where
      go x =
        [ (c, timesR w w')
          | (y, w) <- f x,
            (c, w') <- case y of
              Left a -> go (Left a)
              Right c -> [(c, oneR)]
        ]

-- ---------------------------------------------------------------------------
-- Source parser choice and runner
-- ---------------------------------------------------------------------------

-- | Weighted choice for a list-monad parser.  On a left success the body
-- emits both a 'Right' success edge and a 'Left' feedback edge (with the
-- original stream), so the trace can also explore the right branch.
altW ::
  forall f s a.
  (Uncons f s) =>
  Parser [] f s a ->
  Parser [] f s a ->
  Parser [] f s a
altW (Parser p1) (Parser p2) = Parser $ C.trace $ C.Lift $ Kleisli $ \case
  Right s -> runKleisli (C.run p1) s >>= \case
    That s' -> [Left s']
    res -> [Right res, Left s]
  Left s -> Right <$> runKleisli (C.run p2) s

-- | Interpret a list-monad parser into the weighted target and collect the
-- weighted results for a given input.
runSemiringParser ::
  forall r f s a.
  (Semiring r, Uncons f s) =>
  Parser [] f s a ->
  f ->
  [(These a f, r)]
runSemiringParser p input =
  runSemiringKleisli (C.bind emb (unParser p)) input
  where
    emb :: Kleisli [] x y -> SemiringKleisli r x y
    emb (Kleisli g) = SemiringKleisli $ \x -> [ (y, oneR) | y <- g x ]

-- | Sum the weights for all outputs, ignoring the outputs themselves.
totalWeight :: (Semiring r) => [(x, r)] -> r
totalWeight = foldr (plusR . snd) zeroR

-- | Smoke test: count the parses of @"ab" | "a"@ on input @"ab"@.
--
-- >>> demo
-- [(These "ab" "",Count 1),(These "a" "b",Count 1)]
-- Count 2
demo :: IO ()
demo = do
  let p = string "ab" `altW` string "a" :: Parser [] String Char String
  let results = runSemiringParser @Count p "ab"
  print results
  print $ totalWeight results
