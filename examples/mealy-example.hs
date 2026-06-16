{-# LANGUAGE ImportQualifiedPost #-}

-- | Example: Mealy machines as Circuits.
--
-- This demonstrates 'Circuit.Mealy' by building a stream processor
-- that parses numbers and computes running statistics.
module Main where

import Circuit.Mealy
import Circuit.Parser (These (..), runParser)
import Circuit.Parser.Primitives (double)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.Maybe (mapMaybe)

-- | Parse a comma-separated double.
parseDouble :: ByteString -> Maybe Double
parseDouble bs = case runParser double bs of
  These d _ -> Just d
  This d -> Just d
  That _ -> Nothing

-- | A MealyC that sums its inputs.
sumC :: MealyC Double Double Double
sumC = fromStep (+) id

-- | A MealyC that counts inputs.
countC :: MealyC Int Double Int
countC = fromStep (\c _ -> c + 1) id

-- | A MealyC that tracks the running average.
-- State is (sum, count), output is average.
avgC :: MealyC (Double, Int) Double Double
avgC = fromStep step extract
  where
    step (s, c) x = (s + x, c + 1)
    extract (s, c) = if c == 0 then 0 else s / fromIntegral c

-- Note: sumC and countC have different state types, so they can't be
-- composed with (.) directly. Category (MealyC s) requires the same s.
-- For parallel state, define a combined state machine like avgC above.

main :: IO ()
main = do
  let inputs = [1, 2, 3, 4, 5] :: [Double]

  putStrLn "=== sumC ==="
  print $ scanC sumC 0 inputs

  putStrLn "=== countC ==="
  print $ scanC countC 0 inputs

  putStrLn "=== avgC ==="
  print $ scanC avgC (0, 0) inputs

  -- Parse a stream of comma-separated numbers and compute running avg
  let csv = C.pack "1.5,2.5,3.5,4.5,5.5"
      nums = Data.Maybe.mapMaybe parseDouble (C.split ',' csv)

  putStrLn "=== parsed + avgC ==="
  print nums
  print $ scanC avgC (0, 0) nums
