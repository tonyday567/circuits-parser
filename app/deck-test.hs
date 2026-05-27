{-# LANGUAGE OverloadedStrings #-}

module Main where

import Circuit.Deck

plain, inline, bare, mixed, bad :: String
plain  = "markdown \x27E1 general\n  \x27DC words know themselves by the company they keep\n  \x27DC collusion without collision.\n  \x27DC we grind with our minds.\n"
inline = "**surface** \x27DC shared tokens, shared rules, grounded in reality\n**no separation** \x27DC syntax and semantics flow together\n**commons** \x27DC markdown is the shared medium for humans and agents\n"
bare   = "\x1F7E3 desktop setup; tailscale(rose @ 100.107.55.113); terminus, tmux\n\x1F4CC locks \x1F45F  ~/haskell/circuits/\n\x1F4AC requests\n"
mixed  = "**card** \x27DC collaborative communication unit\n  \x27DC strategy \x27DC what we intend to do, written collaboratively\n  \x27DC flow \x27DC marks topology: position, branch, failure\n  \x27DC artifact \x27DC tested example, pattern recipe, selective memory\n\n\x29C8 cards are how we encode intent.\n"
bad    = "  \x27DC this has nothing to elaborate\n"

main :: IO ()
main = do
  let tests = [ ("plain", plain)
              , ("inline", inline)
              , ("bare", bare)
              , ("mixed", mixed)
              , ("bad", bad)
              ]
  mapM_ (\(name, input) -> do
    putStrLn $ "--- " ++ name ++ " ---"
    putStrLn input
    case parseCard input of
      Right card -> print card
      Left err -> putStrLn $ "PARSE ERROR: " ++ err
    ) tests
