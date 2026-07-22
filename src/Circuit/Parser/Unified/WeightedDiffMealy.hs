{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Weighted parser syntax compiled to a 'DiffMealy'.
--
-- This is the differentiable backend for the regular/applicative fragment.
-- A parser is built from weighted primitives and alternatives; consuming a
-- token transitions by Brzozowski derivative and multiplies an accumulator by
-- the token's parameterised weight.  Extraction returns the accumulated weight
-- when the current derivative is nullable, so the output is the inside value
-- and the 'DiffMealy' scan gives its gradient w.r.t. the weights.
--
-- === doctests
--
-- >>> import Circuit.Parser.Unified.WeightedDiffMealy (diffMealyOutsideDemo)
-- >>> diffMealyOutsideDemo
-- (6.0,ABParam {abWa = 3.0, abWb = 2.0})
-- (2.0,ABParam {abWa = 1.0, abWb = 0.0})
module Circuit.Parser.Unified.WeightedDiffMealy
  ( -- * Weighted parser syntax
    WParser (..),
    wstring,

    -- * DiffMealy compiler
    DiffState (..),
    compileDiffMealy,

    -- * Demo
    ABParam (..),
    abDiffMealy,
    diffMealyOutsideDemo,
  )
where

import Circuit.Parser.Unified.MealyProbe (ABParam (..), Token (..), paramFold)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Mealy.Diff (DiffMealy (..))
import NumHask.Algebra.Additive qualified as Add
import NumHask.Algebra.Multiplicative qualified as Mul
import NumHask.Diff (Diff, pattern Diff)
import Prelude hiding (id, (.))

-- $setup
-- >>> import Circuit.Parser.Unified.WeightedDiffMealy (diffMealyOutsideDemo)

-- ---------------------------------------------------------------------------
-- Weighted parser syntax
-- ---------------------------------------------------------------------------

-- | A small regular/applicative parser with per-token weights supplied
-- externally.  Values are carried for the same reason they are in any parser
-- syntax: they let us name the result of a parse, but only the /weights/ are
-- differentiated.
data WParser s a where
  WPure :: a -> WParser s a
  WFail :: WParser s a
  WAny :: WParser s s
  WSatisfy :: (s -> Bool) -> WParser s s
  WChar :: (Eq s) => s -> WParser s s
  WString :: (Eq s) => [s] -> WParser s [s]
  WAlt :: WParser s a -> WParser s a -> WParser s a
  WFmap :: (a -> b) -> WParser s a -> WParser s b

-- | Convenience constructor for matching a fixed string.
wstring :: (Eq s) => [s] -> WParser s [s]
wstring = WString

-- | Brzozowski derivative for the weighted fragment.
derive :: (Eq s) => s -> WParser s a -> WParser s a
derive _ (WPure _) = WFail
derive _ WFail = WFail
derive _ WAny = WAny
derive c (WSatisfy p) = if p c then WPure c else WFail
derive c (WChar d) = if c == d then WPure d else WFail
derive _ (WString []) = WFail
derive c (WString (d : ds)) =
  if c == d
    then if null ds then WPure [d] else WFmap (d :) (WString ds)
    else WFail
derive c (WAlt p1 p2) = WAlt (derive c p1) (derive c p2)
derive c (WFmap g p) = WFmap g (derive c p)

-- | Whether a parser can never succeed on any remaining input.
isFail :: WParser s a -> Bool
isFail WFail = True
isFail (WAlt p1 p2) = isFail p1 && isFail p2
isFail (WFmap _ p) = isFail p
isFail _ = False

-- | Total weight of nullable parses of a parser (ignoring result values).
nullableWeight :: (Add.Additive r, Mul.Multiplicative r) => WParser s a -> r
nullableWeight (WPure _) = Mul.one
nullableWeight WFail = Add.zero
nullableWeight WAny = Add.zero
nullableWeight (WSatisfy _) = Add.zero
nullableWeight (WChar _) = Add.zero
nullableWeight (WString cs) = if null cs then Mul.one else Add.zero
nullableWeight (WAlt p1 p2) = nullableWeight p1 Add.+ nullableWeight p2
nullableWeight (WFmap _ p) = nullableWeight p

-- ---------------------------------------------------------------------------
-- DiffMealy compiler
-- ---------------------------------------------------------------------------

-- | State of the differentiable machine: an accumulated weight and the current
-- derivative.  The 'Maybe' mode makes the additive instance easy: 'zero' is
-- "no active parse" and addition keeps the left-hand mode.
newtype DiffState r q = DiffState {unDiffState :: (r, Maybe q)}

instance (Add.Additive r) => Add.Additive (DiffState r q) where
  zero = DiffState (Add.zero, Nothing)
  DiffState (w, q) + DiffState (w', q') = DiffState (w Add.+ w', q <|> q')

-- | Compile a weighted parser into a 'DiffMealy'.
--
-- The caller supplies a differentiable token-weight function
-- @'Diff' (p, 'Token') r@.  The actual token is selected at runtime, but the
-- gradient w.r.t. the parameter record @p@ is computed by reverse mode through
-- the scan.
compileDiffMealy ::
  forall p r a.
  (Add.Additive p, Add.Additive r, Mul.Multiplicative r) =>
  (Char -> Diff (p, Token) r) ->
  WParser Char a ->
  DiffMealy (DiffState r (WParser Char a)) (p, Token) r
compileDiffMealy weight p0 =
  DiffMealy
    { dInject = injectDiff,
      dStep = stepDiff,
      dExtract = extractDiff
    }
  where
    injectDiff :: Diff (p, Token) (DiffState r (WParser Char a))
    injectDiff = Diff $ \(param, Token c) ->
      let q' = derive c p0
          Diff wDiff = weight c
          (wToken, pw) = wDiff (param, Token c)
          w' = if isFail q' then Add.zero else wToken
       in ( DiffState (w', Just q'),
            \dstate ->
              let dw = fst (unDiffState dstate)
                  dwToken = if isFail q' then Add.zero else dw
                  (dparam, dToken) = pw dwToken
               in (dparam, dToken)
          )

    stepDiff :: Diff (DiffState r (WParser Char a), (p, Token)) (DiffState r (WParser Char a))
    stepDiff = Diff $ \(DiffState (w, mq), (param, Token c)) ->
      let q = fromMaybe WFail mq
          q'' = derive c q
          Diff wDiff = weight c
          (wToken, pw) = wDiff (param, Token c)
          w' = if isFail q'' then Add.zero else w Mul.* wToken
       in ( DiffState (w', Just q''),
            \dstate ->
              let dw = fst (unDiffState dstate)
                  dwToW = if isFail q'' then Add.zero else dw Mul.* wToken
                  dwToToken = if isFail q'' then Add.zero else dw Mul.* w
                  (dparam, dToken) = pw dwToToken
               in (DiffState (dwToW, mq), (dparam, dToken))
          )

    extractDiff :: Diff (DiffState r (WParser Char a)) r
    extractDiff = Diff $ \(DiffState (w, mq)) ->
      let nw = maybe Add.zero nullableWeight mq
          y = w Mul.* nw
       in (y, \dy -> DiffState (dy Mul.* nw, mq))

-- ---------------------------------------------------------------------------
-- Demo: "ab" | "a" with per-token weights
-- ---------------------------------------------------------------------------

-- | Differentiable weight for the two-token alphabet used in
-- 'Circuit.Parser.Unified.SemiringProbe.outsideDemo'.
abWeight :: Char -> Diff (ABParam, Token) Double
abWeight 'a' =
  Diff $ \(p, _) ->
    ( abWa p,
      \dw -> (ABParam dw 0, Token '\0')
    )
abWeight 'b' =
  Diff $ \(p, _) ->
    ( abWb p,
      \dw -> (ABParam 0 dw, Token '\0')
    )
abWeight _ = Diff $ const (0, const (Add.zero, Token '\0'))

-- | Weighted parser for @"ab" | "a"@.
abWParser :: WParser Char String
abWParser = WString "ab" `WAlt` WString "a"

-- | Differentiable recogniser for @"ab" | "a"@.
abDiffMealy :: DiffMealy (DiffState Double (WParser Char String)) (ABParam, Token) Double
abDiffMealy = compileDiffMealy abWeight abWParser

-- | Gradient bridge: the 'DiffMealy' scan reproduces the outside values.
--
-- >>> diffMealyOutsideDemo
-- (6.0,ABParam {abWa = 3.0, abWb = 2.0})
-- (2.0,ABParam {abWa = 1.0, abWb = 0.0})
diffMealyOutsideDemo :: IO ()
diffMealyOutsideDemo = do
  let p = ABParam 2 3
  print (paramFold abDiffMealy p "ab")
  print (paramFold abDiffMealy p "a")
