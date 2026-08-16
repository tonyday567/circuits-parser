{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Coalgebraic compiler: parser syntax trees become Process state machines.
--
-- A parser in 'Circuit.Parser.Syntax' is already a relation.  This
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
-- >>> import Circuit.Stats (scan, fold)
-- >>> import Circuit.Parser.Syntax (charS, stringS, manyS)
-- >>> import Control.Applicative ((<|>))
--
-- >>> scan (compileProcess (stringS "ab" <|> stringS "a" :: ParserSyntax String Char String)) "ab"
-- [Just "a",Just "ab"]
--
-- >>> fold (compileProcess (stringS "ab" <|> stringS "a" :: ParserSyntax String Char String)) "ab"
-- Just "ab"
--
-- >>> scan (compileProcess (manyS (charS 'a') :: ParserSyntax String Char String)) "aaab"
-- [Just "a",Just "aa",Just "aaa",Nothing]
module Circuit.Parser.ProcessCompiler
  ( -- * Plain Process compiler
    compileProcess,
    compileProcessWithInput,
  )
where

import Circuit.Parser (Uncons (..))
import Circuit.Parser.Syntax (ParserSyntax, derive, nullableValue)
import Circuit.Stats (Process (..))
import Data.Proxy (Proxy (..))
import Data.These (These (..))
import Prelude

-- $setup
-- >>> import Circuit.Stats (scan, fold)
-- >>> import Circuit.Parser.Syntax (ParserSyntax, charS, stringS, manyS)
-- >>> import Control.Applicative ((<|>))

-- | Drop one element from a stream, returning the empty stream if the element
-- was the last one.
dropOne :: forall f s. (Uncons f s) => Proxy s -> f -> f
dropOne _ xs = case uncons @f @s xs of
  These _ rest -> rest
  This _ -> nil @f @s
  That _ -> xs

-- ---------------------------------------------------------------------------
-- Plain Process compiler
-- ---------------------------------------------------------------------------

-- | Compile a parser into a Process machine whose output is the parse result of
-- the prefix consumed so far.
--
-- The machine state is the Brzozowski derivative of the original parser after
-- the tokens seen to date.  Extraction succeeds exactly when that derivative
-- is nullable.
--
-- >>> scan (compileProcess (stringS "ab" <|> stringS "a" :: ParserSyntax String Char String)) "ab"
-- [Just "a",Just "ab"]
compileProcess ::
  (Eq s, Uncons f s) =>
  ParserSyntax f s a ->
  Process s (Maybe a)
compileProcess p0 = Process inject step extract
  where
    inject c = derive c p0
    step p c = derive c p
    extract = nullableValue

-- | Compile a parser into a Process machine tied to a known input stream.
--
-- The state carries the current derivative /and/ the remaining suffix of the
-- input.  This lets extraction return both a parse result and the leftover
-- stream, matching the usual parser output shape.
--
-- Because a 'Process' has no explicit initial state, the input stream is
-- captured at compile time and threaded through 'inject' and 'step'.
--
-- >>> scan (compileProcessWithInput "ab" (stringS "ab" <|> stringS "a" :: ParserSyntax String Char String)) "ab"
-- [Just ("a","b"),Just ("ab","")]
compileProcessWithInput ::
  forall f s a.
  (Eq s, Uncons f s) =>
  f ->
  ParserSyntax f s a ->
  Process s (Maybe (a, f))
compileProcessWithInput input p0 = Process inject step extract
  where
    inject c = (derive c p0, dropOne (Proxy @s) input)
    step (p, rest) c = (derive c p, dropOne (Proxy @s) rest)
    extract (p, rest) = (,rest) <$> nullableValue p
