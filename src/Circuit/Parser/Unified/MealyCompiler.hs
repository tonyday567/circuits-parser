{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Coalgebraic compiler: parser syntax trees become Mealy state machines.
--
-- A parser in 'Circuit.Parser.Unified.Syntax' is already a relation.  This
-- module turns that relation into a coalgebra by Brzozowski derivation:
--
-- * machine state = a parser syntax tree;
-- * input token   = the next stream element;
-- * transition    = Brzozowski derivative of the current tree;
-- * output        = the nullable value of the current tree, if any.
--
-- The regular/applicative fragment is fully supported: primitives, '@<|>@',
-- '@<*@', 'fmap', and 'many'.  'CombBind' is rejected because it is dependent
-- composition.  Explicit 'SigCompose' nodes are also rejected; use the
-- 'Functor'/'Applicative'/'Alternative' constructors instead.
--
-- The differentiable version uses the same derivative coalgebra, but the
-- machine carries an accumulated semiring weight and a caller-supplied
-- differentiable token-weight function.  Extraction multiplies the accumulated
-- weight by the nullable weight of the current derivative, so the 'DiffMealy'
-- scan gives the inside value and its gradient w.r.t. the parameters.
--
-- === doctests
--
-- >>> import Data.Mealy (scan, fold)
-- >>> import Circuit.Parser.Unified.Syntax (charS, stringS, manyS)
-- >>> import Control.Applicative ((<|>))
--
-- >>> scan (compileMealy (stringS "ab" <|> stringS "a" :: ParserSyntax String Char String)) "ab"
-- [Just "a",Just "ab"]
--
-- >>> fold (compileMealy (stringS "ab" <|> stringS "a" :: ParserSyntax String Char String)) "ab"
-- Just "ab"
--
-- >>> scan (compileMealy (manyS (charS 'a') :: ParserSyntax String Char String)) "aaab"
-- [Just "a",Just "aa",Just "aaa",Nothing]
--
-- >>> mealyCompilerOutsideDemo
-- (6.0,ABParam {abWa = 3.0, abWb = 2.0})
-- (2.0,ABParam {abWa = 1.0, abWb = 0.0})
module Circuit.Parser.Unified.MealyCompiler
  ( -- * Plain Mealy compiler
    compileMealy,
    compileMealyWithInput,

    -- * Differentiable machine
    DiffState (..),
    compileDiffMealy,

    -- * Demo
    mealyCompilerOutsideDemo,
  )
where

import Circuit.Parser.Unified (Uncons (..))
import Circuit.Parser.Unified.MealyProbe (ABParam (..), Token (..), paramFold)
import Circuit.Parser.Unified.Syntax
  ( ParserSyntax (..),
    derive,
    nullableValue,
    stringS,
  )
import Control.Applicative (Alternative (empty), (<|>))
import Data.Maybe (fromMaybe)
import Data.Mealy (Mealy (..))
import Data.Mealy.Diff (DiffMealy (..))
import Data.Proxy (Proxy (..))
import Data.These (These (..))
import NumHask.Algebra.Additive qualified as Add
import NumHask.Algebra.Multiplicative qualified as Mul
import NumHask.Diff (Diff, Diff' (..))
import Prelude

-- $setup
-- >>> import Data.Mealy (scan, fold)
-- >>> import Circuit.Parser.Unified.Syntax (ParserSyntax, charS, stringS, manyS)
-- >>> import Control.Applicative ((<|>))

-- | Drop one element from a stream, returning the empty stream if the element
-- was the last one.
dropOne :: forall f s. (Uncons f s) => Proxy s -> f -> f
dropOne _ xs = case uncons @f @s xs of
  These _ rest -> rest
  This _ -> nil @f @s
  That _ -> xs

-- ---------------------------------------------------------------------------
-- Plain Mealy compiler
-- ---------------------------------------------------------------------------

-- | Compile a parser into a Mealy machine whose output is the parse result of
-- the prefix consumed so far.
--
-- The machine state is the Brzozowski derivative of the original parser after
-- the tokens seen to date.  Extraction succeeds exactly when that derivative
-- is nullable.
--
-- >>> scan (compileMealy (stringS "ab" <|> stringS "a" :: ParserSyntax String Char String)) "ab"
-- [Just "a",Just "ab"]
compileMealy ::
  (Eq s, Uncons f s) =>
  ParserSyntax f s a ->
  Mealy s (Maybe a)
compileMealy p0 = Mealy inject step extract
  where
    inject c = derive c p0
    step p c = derive c p
    extract = nullableValue

-- | Compile a parser into a Mealy machine tied to a known input stream.
--
-- The state carries the current derivative /and/ the remaining suffix of the
-- input.  This lets extraction return both a parse result and the leftover
-- stream, matching the usual parser output shape.
--
-- Because a 'Mealy' has no explicit initial state, the input stream is
-- captured at compile time and threaded through 'inject' and 'step'.
--
-- >>> scan (compileMealyWithInput "ab" (stringS "ab" <|> stringS "a" :: ParserSyntax String Char String)) "ab"
-- [Just ("a","b"),Just ("ab","")]
compileMealyWithInput ::
  forall f s a.
  (Eq s, Uncons f s) =>
  f ->
  ParserSyntax f s a ->
  Mealy s (Maybe (a, f))
compileMealyWithInput input p0 = Mealy inject step extract
  where
    inject c = (derive c p0, dropOne (Proxy @s) input)
    step (p, rest) c = (derive c p, dropOne (Proxy @s) rest)
    extract (p, rest) = (,rest) <$> nullableValue p

-- ---------------------------------------------------------------------------
-- Differentiable machine
-- ---------------------------------------------------------------------------

-- | State of the differentiable machine: an accumulated weight and the current
-- derivative.  The 'Maybe' mode makes the additive instance easy: 'zero' is
-- "no active parse" and addition keeps the left-hand mode.
newtype DiffState r q = DiffState {unDiffState :: (r, Maybe q)}

instance (Add.Additive r) => Add.Additive (DiffState r q) where
  zero = DiffState (Add.zero, Nothing)
  DiffState (w, q) + DiffState (w', q') = DiffState (w Add.+ w', q <|> q')

-- | Total weight of nullable parses of a parser (ignoring result values).
nullableWeight ::
  (Add.Additive r, Mul.Multiplicative r) =>
  ParserSyntax String Char a ->
  r
nullableWeight = maybe Add.zero (const Mul.one) . nullableValue

-- | Compile a weighted parser syntax tree into a 'DiffMealy'.
--
-- The caller supplies a differentiable token-weight function
-- @'Diff' (p, 'Token') r@.  The actual token is selected at runtime, but the
-- gradient w.r.t. the parameter record @p@ is computed by reverse mode through
-- the scan.
compileDiffMealy ::
  forall p r a.
  (Add.Additive p, Add.Additive r, Mul.Multiplicative r) =>
  (Char -> Diff (p, Token) r) ->
  ParserSyntax String Char a ->
  DiffMealy (DiffState r (ParserSyntax String Char a)) (p, Token) r
compileDiffMealy weight p0 =
  DiffMealy
    { dInject = injectDiff,
      dStep = stepDiff,
      dExtract = extractDiff
    }
  where
    injectDiff :: Diff (p, Token) (DiffState r (ParserSyntax String Char a))
    injectDiff = Diff' $ \(param, Token c) ->
      let q' = derive c p0
          Diff' wDiff = weight c
          (wToken, pw) = wDiff (param, Token c)
       in ( DiffState (wToken, Just q'),
            \dstate ->
              let dw = fst (unDiffState dstate)
                  (dparam, dToken) = pw dw
               in (dparam, dToken)
          )

    stepDiff ::
      Diff
        (DiffState r (ParserSyntax String Char a), (p, Token))
        (DiffState r (ParserSyntax String Char a))
    stepDiff = Diff' $ \(DiffState (w, mq), (param, Token c)) ->
      let q = fromMaybe empty mq
          q'' = derive c q
          Diff' wDiff = weight c
          (wToken, pw) = wDiff (param, Token c)
          w' = w Mul.* wToken
       in ( DiffState (w', Just q''),
            \dstate ->
              let dw = fst (unDiffState dstate)
                  dwToW = dw Mul.* wToken
                  dwToToken = dw Mul.* w
                  (dparam, dToken) = pw dwToToken
               in (DiffState (dwToW, mq), (dparam, dToken))
          )

    extractDiff :: Diff (DiffState r (ParserSyntax String Char a)) r
    extractDiff = Diff' $ \(DiffState (w, mq)) ->
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
  Diff' $ \(p, _) ->
    ( abWa p,
      \dw -> (ABParam dw 0, Token '\0')
    )
abWeight 'b' =
  Diff' $ \(p, _) ->
    ( abWb p,
      \dw -> (ABParam 0 dw, Token '\0')
    )
abWeight _ = Diff' $ const (0, const (Add.zero, Token '\0'))

-- | Weighted parser syntax for @"ab" | "a"@.
abParserSyntax :: ParserSyntax String Char String
abParserSyntax = stringS "ab" <|> stringS "a"

-- | Differentiable recogniser for @"ab" | "a"@, compiled from syntax.
abDiffMealyCompiled ::
  DiffMealy
    (DiffState Double (ParserSyntax String Char String))
    (ABParam, Token)
    Double
abDiffMealyCompiled = compileDiffMealy abWeight abParserSyntax

-- | Gradient bridge: the compiled 'DiffMealy' reproduces the outside values.
--
-- >>> mealyCompilerOutsideDemo
-- (6.0,ABParam {abWa = 3.0, abWb = 2.0})
-- (2.0,ABParam {abWa = 1.0, abWb = 0.0})
mealyCompilerOutsideDemo :: IO ()
mealyCompilerOutsideDemo = do
  let p = ABParam 2 3
  print (paramFold abDiffMealyCompiled p "ab")
  print (paramFold abDiffMealyCompiled p "a")
