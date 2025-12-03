module Main where

import Control.Concurrent.STM (check)
import Data.Array (Ix (range))
import Data.Bool (bool)
import Data.Char (isDigit)
import System.Environment (getArgs)

-- Part 1
{-
- Invalid ids follow the pattern of doubling up the same digit or sequence of digits i.e. 11, 22 are invalid but 12 or 202 are valid
- Split on ','
- for each range string
  - convert the left and right set of characters into numbers to get the actual range to interate over
- for each number in the range from A to B ceck if the number x is made up of a "doubling" pattern as oulined above
-  keep a count of how many of these numbers are found for each range
-}

splitOn :: Char -> String -> [String]
splitOn d s =
  let (chunks, current) =
        foldl
          ( \(acc, cur) x ->
              if x == d
                then (acc ++ [cur], "")
                else (acc, cur ++ [x])
          )
          ([], "")
          s
   in chunks ++ [current]

parseRange :: String -> (Int, Int)
parseRange s =
  case splitOn '-' s of
    [a, b] -> (read a, read b)
    _ -> error ("invalid range: " ++ s)

parseInput :: String -> [(Int, Int)]
parseInput input = map parseRange (splitOn ',' input)

checkDouble :: String -> Bool
checkDouble s =
  let (left, right) = splitAt (length s `div` 2) s
   in left == right

doublesInRange :: (Int, Int) -> [Int]
doublesInRange r =
  let nums = range r
   in filter (checkRepeated . show) nums

-- Part 2

checkRepeated :: String -> Bool
checkRepeated str =
  let n = length str
      maxPatternLength = n `div` 2
      patternLengths = [1 .. maxPatternLength]
   in any (isValidPattern str n) patternLengths

-- Checks if a pattern/substring can
-- "fit" into the original string by checking that its length divides into the original strings length (i.e. n % d == 0 is )
isValidPattern :: String -> Int -> Int -> Bool
isValidPattern str strLen patternLen =
  (strLen `mod` patternLen == 0)
    && ( let p = take patternLen str
             k = strLen `div` patternLen
          in concat (replicate k p) == str
       )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      let parsed = parseInput contents
      let total = foldl (\acc xs -> acc + sum xs) 0 (filter (not . null) (map doublesInRange parsed))
      print total
    _ -> putStrLn "Usage: program filepath"