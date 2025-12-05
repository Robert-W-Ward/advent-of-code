{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Array (Array, assocs, bounds, listArray, (!), (//))
import Data.Ix (inRange)
import System.Environment (getArgs)

type Coords = (Int, Int)

type Grid = Array Coords Char

toGrid :: [String] -> Grid
toGrid rows =
  let h = length rows
      w = length (head rows)
   in listArray ((0, 0), (h - 1, w - 1)) (concat rows)

parseInput :: String -> Grid
parseInput = toGrid . lines

neighborOffsets :: [Coords]
neighborOffsets =
  [ (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1)
  ]

neighbors :: Grid -> Coords -> [Coords]
neighbors grid (r, c) =
  let bnds = bounds grid
      addOffset (dr, dc) = (r + dr, c + dc)
   in filter (inRange bnds) (map addOffset neighborOffsets)

isAccessible :: Grid -> Coords -> Bool
isAccessible grid pos =
  let ch = grid ! pos
   in ch == '@' && countAtNeighbors < 4
  where
    ns = neighbors grid pos
    countAtNeighbors =
      length (filter (\p -> grid ! p == '@') ns)

solvePart1 :: Grid -> Int
solvePart1 = countAccessible
  where
    countAccessible grid =
      foldr step 0 (assocs grid)
      where
        step (pos, ch) acc
          | ch == '@' && isAccessible grid pos = acc + 1
          | otherwise = acc

findAccessible :: Grid -> [Coords]
findAccessible grid =
  [pos | (pos, char) <- assocs grid, char == '@', isAccessible grid pos]

remove :: [Coords] -> Grid -> Grid
remove positions grid =
  grid // [(pos, '.') | pos <- positions]

iterUntilNoChange :: (Int, Grid) -> (Int, Grid)
iterUntilNoChange (count, grid) =
  let accessible = findAccessible grid
      newCount = count + length accessible
   in if null accessible
        then (newCount, grid)
        else iterUntilNoChange (newCount, remove accessible grid)

solvePart2 :: Grid -> Int
solvePart2 g = fst (iterUntilNoChange (0, g))

main :: IO ()
main = do
  getArgs >>= \case
    [filePath] ->
      readFile filePath >>= \contents ->
        let grid = parseInput contents
            answer = solvePart2 grid
         in print answer
    _ -> putStrLn "Usage: program filepath"
