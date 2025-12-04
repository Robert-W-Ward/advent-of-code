{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent.STM (check)
import Data.Array (Ix (range))
import Data.Bool (bool)
import Data.Char (digitToInt, isDigit)
import Data.List (sortBy, subsequences)
import System.Environment (getArgs)


bestPair :: String -> Int
bestPair xs =
  let digits = map digitToInt xs

      -- step :: Int -> (bestRight, bestPairSoFar) -> (bestRight', bestPair')
      step d (bestRight, bestPairSoFar) =
        let bestPair' =
              if bestRight == 0
                then bestPairSoFar
                else max bestPairSoFar (10 * d + bestRight)
            bestRight' = max bestRight d
         in (bestRight', bestPair')

      -- initial state: no right digit yet → bestRight = 0, bestPair = 0
      (_, finalBestPair) = foldr step (0, 0) digits
   in finalBestPair

duoDecimals :: String -> [String]
duoDecimals x = filter (\sub -> length sub == 12) (subsequences x)

bestSubsequence :: String -> Int
bestSubsequence xs =
  let k = 12
      bestDigits = pickK k xs
   in read bestDigits

-- pickK k xs = lexicographically largest subsequence of xs of length k
pickK :: Int -> String -> String
pickK k xs =
  let n = length xs
      toDrop0 = n - k
   in take k (reverse (go toDrop0 [] xs))
  where
    go :: Int -> [Char] -> [Char] -> [Char]
    go _ acc [] = acc
    go d acc (c : cs)
      | d > 0,
        (a : as) <- acc,
        a < c =
          go (d - 1) as (c : cs) 
      | otherwise =
          go d (c : acc) cs 

parseInput :: String -> [String]
parseInput = lines

-- Part 1
solvePart1 :: [String] -> Int
solvePart1 xs = sum (map bestPair xs)

-- Part 2
solvePart2 :: [String] -> Int
solvePart2 xs = sum (map bestSubsequence xs)

main :: IO ()
main = do
  getArgs >>= \case
    [filePath] ->
      readFile filePath >>= \contents ->
        let result = parseInput contents
            answer = solvePart2 result
         in print answer
    _ -> putStrLn "Usage: program filepath"