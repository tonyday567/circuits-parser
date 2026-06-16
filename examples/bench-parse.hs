{-# LANGUAGE ImportQualifiedPost #-}

-- Benchmark: Circuit.Parser character consumption throughput
module Main where

import Circuit.Parser (anyToken, many, runParser, satisfy, (<|>))
import Circuit.Perf (times)
import Data.ByteString qualified as B
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)

main :: IO ()
main = do
  html <- B.readFile "/tmp/html5-spec.html"
  let slice = 10000
      bs = take slice $ map (toEnum . fromIntegral) (B.unpack html) :: String
      n = 10 :: Int

  putStrLn $ "HTML5 spec (first " ++ show slice ++ " chars), " ++ show n ++ " runs"

  -- 1. many anyToken — consume every char into a list
  (t1, _) <- times n (runParser (many anyToken)) bs
  let avg1 = fromIntegral (sum t1) / fromIntegral (length t1) :: Double
  putStrLn $ "many anyToken:           " ++ fmt avg1 slice

  -- 2. many (satisfy (const True))
  (t2, _) <- times n (runParser (many (satisfy (const True)))) bs
  let avg2 = fromIntegral (sum t2) / fromIntegral (length t2) :: Double
  putStrLn $ "many satisfy(const True): " ++ fmt avg2 slice

  -- 3. Alternation: letter <|> digit <|> punct
  let isLetter c = isAsciiLower c || isAsciiUpper c
      isPunct c = c `elem` (",.;:!?'\"-()[]{}@#$%&*+=<>/\\|~`" :: String)
  (t3, _) <- times n (runParser (many (satisfy isLetter <|> satisfy isDigit <|> satisfy isPunct))) bs
  let avg3 = fromIntegral (sum t3) / fromIntegral (length t3) :: Double
  putStrLn $ "letter|digit|punct:      " ++ fmt avg3 slice

  -- 4. satisfy isLetter only
  (t4, _) <- times n (runParser (many (satisfy isLetter))) bs
  let avg4 = fromIntegral (sum t4) / fromIntegral (length t4) :: Double
  putStrLn $ "many satisfy isLetter:   " ++ fmt avg4 slice

fmt :: Double -> Int -> String
fmt avg chars =
  show (round (avg / 1e3))
    ++ " us, "
    ++ show (fromIntegral (round (avg / fromIntegral chars)) :: Double)
    ++ " ns/char"
