{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent.STM (check)
import Data.Array (Ix (range))
import Data.Bool (bool)
import Data.Char (digitToInt, isDigit)
import System.Environment (getArgs)

{- Two pointers?
- It seems like the best approach is to have tow "pointers" one pointing at the left most element in the list,
one pointing at leftmost + 1 and interate this pointer along until you get to the end of thelist taking the value pointed at
by both pointers as you move along and checking if its greater than the "max" size you have seen if it is replace max otherwise
continue the right pointer (until it reaches then end)

- fold over each element of each list doing the two pointers thing returning the largest value present in the list
- sum over the result of the folds

-}

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

parseInput :: String -> [String]
parseInput = lines

-- Part 1
solvePart1 :: [String] -> Int
solvePart1 xs = sum (map bestPair xs)

-- Part 2
solvePart2 xs = sum (map bestPair xs)

main :: IO ()
main = do
  getArgs >>= \case
    [filePath] ->
      readFile filePath >>= \contents ->
        let result = parseInput contents
            answer = solvePart1 result
         in print answer
    _ -> putStrLn "Usage: program filepath"