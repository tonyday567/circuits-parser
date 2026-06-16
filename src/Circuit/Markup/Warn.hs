{-# LANGUAGE GHC2024 #-}

-- | Warning helpers for markup parsing.
module Circuit.Markup.Warn
  ( warnError,
    warnEither,
    warnMaybe,
    showWarnings,
    concatWarns,
  )
where

import Circuit.Markup.Types (MarkupWarning (..), Warn)
import Control.Category ((>>>))
import Data.Bifunctor
import Data.Bool
import Data.List qualified as List
import Data.These

-- | Convert any warnings to an 'error'
warnError :: Warn a -> a
warnError = these (showWarnings >>> error) id (\xs a -> bool (error (showWarnings xs)) a (null xs))

-- | Returns Left on any warnings
warnEither :: Warn a -> Either [MarkupWarning] a
warnEither = these Left Right (\xs a -> bool (Left xs) (Right a) (null xs))

-- | Returns results, if any, ignoring warnings.
warnMaybe :: Warn a -> Maybe a
warnMaybe = these (const Nothing) Just (\_ a -> Just a)

showWarnings :: [MarkupWarning] -> String
showWarnings = List.nub >>> fmap show >>> unlines

concatWarns :: [Warn [a]] -> Warn [a]
concatWarns rs = case bimap mconcat mconcat $ partitionHereThere rs of
  ([], xs) -> That xs
  (es, []) -> This es
  (es, xs) -> These es xs
