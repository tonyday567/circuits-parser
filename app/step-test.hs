module Main where

import Circuit.Parser.Step
import Control.Applicative ((<|>))

-- Test helpers
p_a, p_b, p_ab, p_offer :: Parser String Char String
p_a = char 'a' $> "a-parsed"
p_b = char 'b' $> "b-parsed"
p_ab = char 'a' >> char 'b' $> "ab-parsed"
p_offer = char 'a' >> offer "got-a"

main :: IO ()
main = do
  putStrLn "=== char 'a' on \"abc\" ==="
  print $ run p_a "abc"

  putStrLn "=== char 'a' on \"xyz\" ==="
  print $ run p_a "xyz"

  putStrLn "=== char 'a' >> char 'b' on \"ax\" ==="
  print $ run p_ab "ax"

  putStrLn "=== p_b <|> p_a on \"abc\" ==="
  print $ run (p_b <|> p_a) "abc"

  putStrLn "=== p_ab <|> p_a on \"ax\" ==="
  print $ run (p_ab <|> p_a) "ax"

  putStrLn "=== (p_offer >> p_b) on \"ab\" ==="
  print $ run (p_offer >> p_b) "ab"

($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)
