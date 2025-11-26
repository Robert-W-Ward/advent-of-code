module Main where

import Data.List (sort, sortOn)
import Data.Ord (Down (..))
import Data.Text qualified
import System.Environment (getArgs)
import Prelude hiding (elem)

--- Part 1:
absDiff :: (Int, Int) -> Int
absDiff (a, b) = abs (a - b)

totalDistance :: [(Int, Int)] -> Int
totalDistance list = sum (map absDiff list)

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortOn Down

toPair :: [Int] -> (Int, Int)
toPair [x, y] = (x, y)

parseInput :: String -> [(Int, Int)]
parseInput = map (toPair . map (read :: String -> Int) . words) . lines

matchPairs :: [(Int, Int)] -> [(Int, Int)]
matchPairs pairs =
  let (xs, ys) = unzip pairs
   in zip (sortDesc xs) (sortDesc ys)

--- Part 2:

similarityScore :: Int -> [Int] -> Int
similarityScore x ys =
  let freq = length (filter (== x) ys)
   in x * freq

totalSimilarityScore :: [(Int, Int)] -> Int
totalSimilarityScore pairs =
  let (xs, ys) = unzip pairs
   in sum (map (`similarityScore` ys) xs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      let parsed = parseInput contents
      let pairs = matchPairs parsed
      let total = totalDistance pairs
      let similarityScore = totalSimilarityScore pairs
      print total
      print similarityScore
    _ -> putStrLn "Usage: program filepath"
