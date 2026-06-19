module Main where

import Circuit.Deck (bareLineP, elabLineP, inlineLineP, lineP)
import Circuit.Parser (runParser, (<|>))

main :: IO ()
main = do
  let input = "first \x27E1 lead\n  \x27DC elab\n" :: String
  putStrLn "=== elabLineP ==="
  print (runParser elabLineP input)
  putStrLn "=== inlineLineP ==="
  print (runParser inlineLineP input)
  putStrLn "=== bareLineP ==="
  print (runParser bareLineP input)
  putStrLn "=== lineP (= elab <|> inline <|> bare) ==="
  print (runParser lineP input)
  putStrLn "=== (elab <|> bare) ==="
  print (runParser (elabLineP <|> bareLineP) input)
