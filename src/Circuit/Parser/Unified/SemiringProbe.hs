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
-- *Current status*
--
-- - Acyclic choice loops work through the standard 'Layer.bind' fold and the
--   'Traced Either (SemiringKleisli r)' instance.
-- - Cyclic loops (e.g. 'many') are demonstrated by 'traceCyclic', which
--   computes the reflexive-transitive closure of the feedback-to-feedback
--   matrix over a 'StarSemiring'.  Integrating that into 'Layer.bind' is
--   blocked by a design tension: the trace needs to compare feedback states
--   ('Eq'), but 'Layer.bind' requires the target category to be 'Discrete',
--   which cannot carry a non-trivial object constraint like 'Eq'.
module Circuit.Parser.Unified.SemiringProbe
  ( Semiring (..),
    StarSemiring (..),
    Count (..),
    SemiringKleisli (..),
    altW,
    runSemiringParser,
    totalWeight,
    traceCyclic,
    demo,
    cyclicDemo,
    manyCharA,
    manyCharAReach,
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
import Data.List (elemIndex, nub, sortOn)
import Data.Maybe (fromMaybe)
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

-- | Semirings with a closure operation @star x = 1 + x + x*x + ...@.
class Semiring r => StarSemiring r where
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
  deriving (Eq, Ord, Show, Num)

instance Semiring Count where
  zeroR = Count 0
  oneR = Count 1
  plusR (Count x) (Count y) = Count (x + y)
  timesR (Count x) (Count y) = Count (x * y)

instance StarSemiring Count where
  star (Count 0) = Count 1
  star _ = error "SemiringProbe.Count.star: infinite derivations"

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

sumR :: (Semiring r) => [r] -> r
sumR = foldr plusR zeroR

-- | Reflexive-transitive closure of an n x n matrix over a star semiring.
--
-- Uses the standard Kleene-algebra update:
-- @C[i][j] += C[i][k] * star(C[k][k]) * C[k][j]@.
-- The result includes the identity matrix.
matrixClosure :: forall r. (StarSemiring r) => [[r]] -> [[r]]
matrixClosure a = addIdentity (foldl' update a [0 .. n - 1])
  where
    n = length a

    update :: [[r]] -> Int -> [[r]]
    update m k =
      let skk = star ((m !! k) !! k)
       in [ [ ((m !! i) !! j)
                `plusR` (((m !! i) !! k) `timesR` skk `timesR` ((m !! k) !! j))
              | j <- [0 .. n - 1]
            ]
          | i <- [0 .. n - 1]
          ]

    addIdentity :: [[r]] -> [[r]]
    addIdentity m =
      [ [ if i == j then oneR `plusR` ((m !! i) !! j) else (m !! i) !! j
          | j <- [0 .. n - 1]
        ]
        | i <- [0 .. n - 1]
      ]

-- | Collect all feedback states reachable from an initial list of states.
feedbackClosure ::
  (Eq a) =>
  (a -> [(a, r)]) ->
  [a] ->
  [a]
feedbackClosure edges = go []
  where
    go seen [] = seen
    go seen (x : xs)
      | x `elem` seen = go seen xs
      | otherwise =
          let next = map fst (edges x)
           in go (x : seen) (xs ++ next)

-- | Cyclic trace for 'SemiringKleisli'.  Requires equality on the feedback
-- channel and on inputs/outputs, so it is provided as a standalone function
-- rather than as the 'Traced Either' instance (which must be 'Discrete' and
-- therefore cannot carry an 'Eq' constraint on objects).
traceCyclic ::
  forall r a b c.
  (StarSemiring r, Eq r, Eq a, Eq b, Eq c) =>
  SemiringKleisli r (Either a b) (Either a c) ->
  SemiringKleisli r b c
traceCyclic (SemiringKleisli f) = SemiringKleisli $ \b0 -> solve (Right b0)
  where
    solve :: Either a b -> [(c, r)]
    solve start =
      let startEdges = f start
          startLefts = [ (a, w) | (Left a, w) <- startEdges ]
          startOuts = [ (c, w) | (Right c, w) <- startEdges ]
          feedbackEdges a' = [ (a'', w) | (Left a'', w) <- f (Left a') ]
          leftStates = feedbackClosure feedbackEdges (map fst startLefts)
          idx a = fromMaybe (error "traceCyclic: feedback state escaped closure") (elemIndex a leftStates)
          n = length leftStates
          adj =
            [ [ sumR [ w | (Left a', w) <- f (Left ai), a' == aj ]
                | aj <- leftStates
              ]
              | ai <- leftStates
            ]
          closure = matrixClosure adj
          leftOuts j =
            [ (outC, w)
              | (Right outC, w) <- f (Left (leftStates !! j))
            ]
          allOuts =
            nub $
              map fst startOuts
                ++ map fst (concat [ leftOuts j | j <- [0 .. n - 1] ])
          total outC =
            let base = sumR [ w | (c, w) <- startOuts, c == outC ]
                via =
                  sumR
                    [ w1 `timesR` ((closure !! i) !! j) `timesR` w2
                      | (a, w1) <- startLefts,
                        let i = idx a,
                        j <- [0 .. n - 1],
                        (c, w2) <- leftOuts j,
                        c == outC
                    ]
             in base `plusR` via
       in [ (outC, total outC) | outC <- allOuts, total outC /= zeroR ]

-- ---------------------------------------------------------------------------
-- Acyclic weighted trace
-- ---------------------------------------------------------------------------

-- | Sum over all 'Right' exits reachable from the initial 'Right' input.
--
-- This implementation is recursive and assumes the feedback graph is acyclic;
-- cycles (e.g. from 'many') will diverge.  Use 'traceCyclic' when cycles are
-- present.
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
-- weighted results for a given input.  This uses the acyclic 'Traced Either'
-- instance, so parsers involving unbounded repetition will diverge.
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

-- | Cyclic trace smoke test: a loop that can either exit immediately or
-- spin arbitrarily many times before exiting.  With the 'Bool' semiring the
-- answer is simply "an exit is reachable".
--
-- >>> cyclicDemo
-- True
cyclicDemo :: Bool
cyclicDemo = any snd (runSemiringKleisli (traceCyclic body) ())
  where
    body :: SemiringKleisli Bool (Either () ()) (Either () ())
    body = SemiringKleisli $ \case
      Right () -> [(Left (), True), (Right (), True)]
      Left () -> [(Left (), True), (Right (), True)]

-- | A more parser-like cyclic relation: @many (char 'a')@ over a 'String'
-- stream.  The feedback channel carries both the remaining stream and the
-- accumulated prefix, so each exit records exactly how many @a@s were
-- consumed.  This exercises 'traceCyclic' with a non-trivial feedback state
-- ('String') and the 'Count' semiring.
--
-- >>> manyCharA "aab"
-- [(These "" "aab",Count 1),(These "a" "ab",Count 1),(These "aa" "b",Count 1)]
manyCharA :: String -> [(These [Char] String, Count)]
manyCharA s = sortOn fst (runSemiringKleisli (traceCyclic body) s)
  where
    body :: SemiringKleisli Count (Either (String, [Char]) String) (Either (String, [Char]) (These [Char] String))
    body = SemiringKleisli $ \case
      Right s' -> step s' []
      Left (s', acc) -> step s' acc
      where
        step s' acc =
          (Right (These acc s'), Count 1)
            : case s' of
              ('a' : s'') -> [(Left (s'', acc ++ "a"), Count 1)]
              _ -> []

-- | Reachability version of 'manyCharA': is there /any/ parse of the loop?
--
-- >>> manyCharAReach "aab"
-- True
manyCharAReach :: String -> Bool
manyCharAReach s = any snd (runSemiringKleisli (traceCyclic body) s)
  where
    body :: SemiringKleisli Bool (Either (String, [Char]) String) (Either (String, [Char]) (These [Char] String))
    body = SemiringKleisli $ \case
      Right s' -> step s' []
      Left (s', acc) -> step s' acc
      where
        step s' acc =
          (Right (These acc s'), True)
            : case s' of
              ('a' : s'') -> [(Left (s'', acc ++ "a"), True)]
              _ -> []
