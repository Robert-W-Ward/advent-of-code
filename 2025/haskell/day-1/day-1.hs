module Main where

import Data.Bool (bool)
import Data.Char (isDigit)
import System.Environment (getArgs)

--- Part 1:

splitLR :: String -> (Char, String)
splitLR (c : cs) = (c, digits)
  where
    digits = takeWhile isDigit cs

toOpAndNumTuple :: String -> (Char, Int)
toOpAndNumTuple input = do
  let pair = splitLR input
  let f :: Char = fst pair
  let l :: String = snd pair

  let parse :: Int = read l
  (f, parse)

toOpAndNumTupleList :: [String] -> [(Char, Int)]
toOpAndNumTupleList = map toOpAndNumTuple

parseInput :: String -> [(Char, Int)]
parseInput input = toOpAndNumTupleList (lines input)

--- Abstract solution
{-
- L = subtract
- R = add
- ## = value to add or subtract
- cannot go below 0 (loops back to 99)
- cannot go above 99 (loops back to 0)
- count how many times the "dial" equals zero
-}

wrap :: Int -> Int
wrap n
  | n > 99 = 0
  | n < 0 = 99
  | otherwise = n

--- Part 2:

delta :: Char -> Int
delta c
  | c == 'L' = -1
  | c == 'R' = 1
  | otherwise = 1

zerosHitThisStep :: Int -> Char -> Int -> Int
zerosHitThisStep curDial dir steps =
  let stepsToFirstZero =
        case dir of
          'R' ->
            let d = (100 - curDial) `mod` 100
             in if d == 0 then 100 else d
          'L' ->
            let d = curDial `mod` 100
             in if d == 0 then 100 else d
          _ -> steps + 1
   in if steps < stepsToFirstZero
        then 0
        else 1 + (steps - stepsToFirstZero) `div` 100

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath

      let parsedContents :: [(Char, Int)]
          parsedContents = parseInput contents

          result :: (Int, Int)
          result =
            foldl
              ( \acc tupe ->
                  let l :: Char = fst tupe
                      r :: Int = snd tupe
                      currentDial :: Int = fst acc
                      zeroCount :: Int = snd acc

                      newDial :: Int =
                        if l == 'L'
                          then (currentDial - r) `mod` 100
                          else (currentDial + r) `mod` 100

                      newZeroCount :: Int =
                        zeroCount + zerosHitThisStep currentDial l r
                   in (newDial, newZeroCount)
              )
              (50, 0)
              parsedContents

      print result
    _ -> putStrLn "Usage: program filepath"
