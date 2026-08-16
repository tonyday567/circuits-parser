{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Example instantiations of the unified parser over different base monads.
--
-- The primitives in "Circuit.Parser" are polymorphic in the base monad
-- @m@. This module shows how to run the same parser syntax under two
-- different interpretations:
--
--   * 'Identity' — attoparsec-style pure parser
--   * @StateT st (ExceptT e n)@ — megaparsec-style state + errors
module Circuit.Parser.Examples
  ( -- * Runners
    runMega,

    -- * Example parsers
    abOrA,
  )
where

import Circuit.Channel (Traced)
import Circuit.Parser
import Control.Arrow (Kleisli (..))
import Control.Monad.Except (ExceptT, runExceptT)
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
