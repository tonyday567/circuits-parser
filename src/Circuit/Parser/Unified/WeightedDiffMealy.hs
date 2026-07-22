{-# LANGUAGE GADTs #-}

-- | A small regular/applicative parser syntax used as a lightweight front-end
-- to the 'Circuit.Parser.Unified.MealyCompiler' backend.
--
-- 'WParser' is exactly the regular fragment of 'ParserSyntax' rebuilt without
-- the full fixed-point machinery.  It exists for demos and teaching; the
-- compiler itself lives in "Circuit.Parser.Unified.MealyCompiler".
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

    -- * DiffMealy compiler (via MealyCompiler)
    compileDiffMealy,

    -- * Demo
    ABParam (..),
    abDiffMealy,
    diffMealyOutsideDemo,
  )
where

import Circuit.Parser.Unified.MealyCompiler qualified as MC
import Circuit.Parser.Unified.MealyProbe (ABParam (..), Token (..), paramFold)
import Circuit.Parser.Unified.Syntax
  ( ParserSyntax,
    anyTokenS,
    charS,
    satisfyS,
    stringS,
  )
import Control.Applicative (Alternative (empty), (<|>))
import Data.Mealy.Diff (DiffMealy (..))
import NumHask.Algebra.Additive qualified as Add
import NumHask.Algebra.Multiplicative qualified as Mul
import NumHask.Diff (Diff, Diff' (..))
import Prelude

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

-- | Embed the small 'WParser' syntax into the shared 'ParserSyntax' tree.
toParserSyntax :: (Eq s) => WParser s a -> ParserSyntax [s] s a
toParserSyntax (WPure a) = pure a
toParserSyntax WFail = empty
toParserSyntax WAny = anyTokenS
toParserSyntax (WSatisfy p) = satisfyS p
toParserSyntax (WChar c) = charS c
toParserSyntax (WString cs) = stringS cs
toParserSyntax (WAlt p1 p2) = toParserSyntax p1 <|> toParserSyntax p2
toParserSyntax (WFmap g p) = g <$> toParserSyntax p

-- | Compile a weighted 'WParser' into a 'DiffMealy' via the shared
-- 'Circuit.Parser.Unified.MealyCompiler' backend.
compileDiffMealy ::
  (Add.Additive p, Add.Additive r, Mul.Multiplicative r) =>
  (Char -> Diff (p, Token) r) ->
  WParser Char a ->
  DiffMealy (MC.DiffState r (ParserSyntax String Char a)) (p, Token) r
compileDiffMealy weight = MC.compileDiffMealy weight . toParserSyntax

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

-- | Weighted parser for @"ab" | "a"@.
abWParser :: WParser Char String
abWParser = WString "ab" `WAlt` WString "a"

-- | Differentiable recogniser for @"ab" | "a"@.
abDiffMealy ::
  DiffMealy
    (MC.DiffState Double (ParserSyntax String Char String))
    (ABParam, Token)
    Double
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
