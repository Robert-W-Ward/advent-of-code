{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (Bifunctor (second), bimap, first)
import Data.Char
import Data.List (transpose)
import System.Environment (getArgs)

parseInput :: String -> [[String]]
parseInput input = map words (lines input)

opToFn :: String -> Int -> Int -> Int
opToFn s = case s of
  "+" -> (+)
  "*" -> (*)
  _ -> error "unknown op"

getOperators :: [[String]] -> [Int -> Int -> Int]
getOperators ops = map opToFn (last ops)

getOperands :: [[String]] -> [[Int]]
getOperands parsed = map (map (read :: String -> Int)) ((tail . reverse) parsed)

solvePart1 :: String -> Int
solvePart1 input =
  let parsed = parseInput input
      operators = getOperators parsed
      operands = getOperands parsed
   in sum (zipWith foldl1 operators (transpose operands))

solvePart2 :: String -> Int
solvePart2 input =
  let rows = lines input
      width = maximum (map length rows)
      padded = map (\row -> row ++ replicate (width - length row) ' ') rows
      cols = transpose padded
      blocks = groupCols cols
      problems = map evalBlock (reverse blocks)
   in sum problems
  where
    isBlankCol :: String -> Bool

    groupCols :: [String] -> [[String]]
    groupCols [] = []
    groupCols xs =
      let xs' = dropWhile isBlankCol xs
       in case break isBlankCol xs' of
            ([], _) -> []
            (grp, rest) -> grp : groupCols rest

    evalBlock :: [String] -> Int
    evalBlock colsInBlock =
      let opChar =
            head
              [ c | col <- colsInBlock, let c = last col, c == '+' || c == '*'
              ]
          op = opToFn [opChar]

          nums =
            [ read (filter isDigit (init col)) :: Int
              | col <- reverse colsInBlock
            ]
       in foldl1 op nums
    isBlankCol = all (== ' ')

main :: IO ()
main =
  getArgs >>= \case
    [filePath] ->
      readFile filePath >>= \contents -> (print . solvePart2) contents
    _ -> putStrLn "Usage: program filepath"
