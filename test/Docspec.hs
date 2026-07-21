module Main where

import System.Exit (exitWith)
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
  (code, out, err) <- readProcessWithExitCode "cabal-docspec" ["circuits-parser"] ""
  putStr out
  putStr err
  exitWith code
