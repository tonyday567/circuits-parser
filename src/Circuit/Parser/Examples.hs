{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Example instantiations of the unified parser over different base monads.
--
-- The primitives in "Circuit.Parser" are polymorphic in the base monad
-- @m@. This module shows how to run the same parser syntax under three
-- different interpretations:
--
--   * 'Identity' — attoparsec-style pure parser
--   * @StateT st (ExceptT e n)@ — megaparsec-style state + errors
--   * @LogicT n@ — LogicT-style nondeterminism
--
-- Note: the LogicT instantiation runs the parser /in/ the LogicT monad, but
-- the 'Loop Either' control structure still commits to the first successful
-- branch. Enumerating /all/ parse trees of the same input requires a different
-- target interpretation of the free 'Loop' (see the unification card).
module Circuit.Parser.Examples
  ( -- * Runners
    runMega,
    runLogic,

    -- * Example parsers
    abOrA,
  )
where

import Circuit.Channel (Traced)
import Circuit.Parser
import Control.Arrow (Kleisli (..))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Logic (LogicT, observeAllT)
import Control.Monad.State (StateT, runStateT)
import Data.These (These (..))

-- | A parser that accepts "ab" or "a".
--
-- Defined once, runnable under any base monad.
abOrA :: (Monad m, Uncons f Char, Traced Either (Kleisli m)) => Parser m f Char String
abOrA = string "ab" <|> string "a"

-- | Megaparsec-style runner: stateful, error-aware.
--
-- The state @st@ can hold offset, tab width, etc. Errors are returned via
-- @ExceptT e n@.
runMega ::
  forall e st n f a.
  (Monad n, Uncons f Char) =>
  Parser (StateT st (ExceptT e n)) f Char a ->
  f ->
  st ->
  n (Either e (These a f, st))
runMega p f st0 = runExceptT (runStateT (runParser p f) st0)

-- | LogicT-style runner: collect all results produced by the base monad.
--
-- Because 'Loop Either' commits to the first successful branch, this usually
-- returns a singleton list. It becomes a list of alternatives when the
-- /parser itself/ uses the base monad's nondeterminism (e.g. via 'lift').
runLogic ::
  forall n f a.
  (Monad n, Uncons f Char) =>
  Parser (LogicT n) f Char a ->
  f ->
  n [These a f]
runLogic p f = observeAllT (runParser p f)
