{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor
import System.Environment (getArgs)
import Data.List (sortOn)

-- This is atrocious and shoud never be done but I wanted to try out a tacit/point free style to solving this at least as much as I could understand

solvePart1 :: String -> Int
solvePart1 =
  ( \(ranges, ints) ->
      -- finds the number of integers in "ints" list that fall within any range in the ranges list
      length
        [ n
          | n <- ints,
            any (\(lo :: Int, hi) -> lo <= n && n <= hi) ranges
        ]
  )
    . bimap
      ( map
          ( \x ->
              let (a, rest) = break (== '-') x
               in (read a, read (tail rest))
          )
      ) -- convert string ranges into tuple of int e.g. "3-5" => (3,5)
      (map read) -- read string integer into actual integer "5" = 5
    . span ('-' `elem`) -- Split the resulting list of strings into a tuple of two lists of strings e.g. (["3-5","10-15"], ["1","5","3"])
    . filter (not . null) -- filter out empty strings i.e. blank lines
    . lines

solvePart2 :: String -> Int
solvePart2 =
  (sum . map (\(lo, hi) -> hi - lo + 1))
    . ( foldr
            ( \(lo, hi) acc ->
                case acc of
                  [] -> [(lo, hi)]
                  (lo', hi') : rs'
                    | hi < lo' - 1 -> (lo, hi) : (lo', hi') : rs'
                    | otherwise -> (min lo lo', max hi hi') : rs'
            )
            [] . sortOn fst
      )
    . map
          ( \x ->
              let (a, rest) = break (== '-') x
               in (read a, read (tail rest))
          )
    . takeWhile ('-' `elem`) -- Split the resulting list of strings into a tuple of two lists of strings e.g. (["3-5","10-15"], ["1","5","3"])
    . filter (not . null) -- filter out empty strings i.e. blank lines
    . lines

main :: IO ()
main =
  getArgs >>= \case
    [filePath] ->
      readFile "input.txt"
        >>= print . solvePart2
    _ -> putStrLn "Usage: program filepath"
