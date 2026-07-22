{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
module Circuit.Parser.Unified.MealyCompiler
  ( -- * Plain Mealy compiler
    compileMealy,
    compileMealyWithInput,

    -- * Differentiable machine (stub)
    Token (..),
    AdditiveSyntax (..),
    compileDiffMealyStub,
  )
where

import Circuit.Parser.Unified (Uncons (..))
import Circuit.Parser.Unified.Syntax
  ( ParserSyntax (..),
    derive,
    nullableValue,
  )
import Control.Applicative (Alternative (empty))
import Data.Mealy (Mealy (..))
import Data.Mealy.Diff (DiffMealy (..))
import Data.Proxy (Proxy (..))
import Data.These (These (..))
import NumHask.Algebra.Additive qualified as Add
import NumHask.Algebra.Multiplicative qualified as Mul
import NumHask.Diff (Diff' (..))
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
-- Differentiable machine stub
-- ---------------------------------------------------------------------------

-- | A token wrapper identical to the one in "Circuit.Parser.Unified.MealyProbe".
--
-- Characters are not parameters, so addition keeps the original token.
newtype Token = Token {unToken :: Char}
  deriving newtype (Eq, Show)

instance Add.Additive Token where
  zero = Token '\0'
  Token _ + Token c = Token c

-- | A parser-syntax state forced into an 'Additive' shape.
--
-- 'DiffMealy' requires an additive state, but a parser syntax tree is not a
-- vector.  This wrapper is the minimal hack that lets the types line up:
-- 'zero' is the failing parser and 'plus' keeps the left-hand state.  A real
-- differentiable compiler needs either a change to the @mealy@ API or a
-- genuine semiring structure on parser states.
newtype AdditiveSyntax f s a = AdditiveSyntax
  {unAdditiveSyntax :: (ParserSyntax f s a, f)}

instance (Uncons f s) => Add.Additive (AdditiveSyntax f s a) where
  zero = AdditiveSyntax (empty, nil @f @s)
  AdditiveSyntax (p, _) + _ = AdditiveSyntax (p, nil @f @s)

-- | Placeholder differentiable machine.
--
-- This demonstrates the API friction rather than solving it: the state is a
-- parser syntax tree wrapped in 'AdditiveSyntax', the output is @1@ when the
-- current derivative is nullable and @0@ otherwise, and all gradients are
-- zero.  Making this useful requires weights on grammar choices and a proper
-- pullback through the derivative coalgebra.
compileDiffMealyStub ::
  (Add.Additive p, Add.Additive b, Mul.Multiplicative b) =>
  ParserSyntax String Char a ->
  DiffMealy (AdditiveSyntax String Char a) (p, Token) b
compileDiffMealyStub p0 =
  DiffMealy
    { dInject =
        Diff' $
          const
            ( AdditiveSyntax (p0, nil @String @Char),
              const (Add.zero, Token '\0')
            ),
      dStep =
        Diff' $ \(AdditiveSyntax (p, rest), (_, Token c)) ->
          let p' = derive c p
              rest' = dropOne (Proxy @Char) rest
           in ( AdditiveSyntax (p', rest'),
                const (Add.zero, (Add.zero, Token '\0'))
              ),
      dExtract =
        Diff' $ \(AdditiveSyntax (p, _)) ->
          let y = maybe Add.zero (const Mul.one) (nullableValue p)
           in (y, const Add.zero)
    }
