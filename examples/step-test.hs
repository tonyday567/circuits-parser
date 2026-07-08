module Main where

import Circuit.Parser.Step
import Control.Applicative ((<|>))

-- Test helpers
pA, pB, pAb, pOffer :: Parser String Char String
pA = char 'a' $> "a-parsed"
pB = char 'b' $> "b-parsed"
pAb = char 'a' >> char 'b' $> "ab-parsed"
pOffer = char 'a' >> offer "got-a"

main :: IO ()
main = do
  putStrLn "=== char 'a' on \"abc\" ==="
  print $ runStep pA "abc"

  putStrLn "=== char 'a' on \"xyz\" ==="
  print $ runStep pA "xyz"

  putStrLn "=== char 'a' >> char 'b' on \"ax\" ==="
  print $ runStep pAb "ax"

  putStrLn "=== pB <|> pA on \"abc\" ==="
  print $ runStep (pB <|> pA) "abc"

  putStrLn "=== pAb <|> pA on \"ax\" ==="
  print $ runStep (pAb <|> pA) "ax"

  putStrLn "=== (pOffer >> pB) on \"ab\" ==="
  print $ runStep (pOffer >> pB) "ab"

($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)
