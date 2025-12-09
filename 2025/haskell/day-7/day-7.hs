{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Set as S

type Grid = [String]
type Col  = Int

findStart :: Grid -> (Int, Int)
findStart rows =
  head
    [ (r, c)
    | (r, row) <- zip [0 ..] rows
    , (c, ch)  <- zip [0 ..] row
    , ch == 'S'
    ]

simulate :: Grid -> Int
simulate rows =
  let h = length rows
      w = length (head rows)
      (startR, startC) = findStart rows

      addCol :: Int -> S.Set Col -> S.Set Col
      addCol c s
        | c < 0 || c >= w = s
        | otherwise       = S.insert c s

      processCell :: Int -> (Int, S.Set Col) -> Col -> (Int, S.Set Col)
      processCell r (cnt, nextSet) c =
        let ch = (rows !! r) !! c
        in case ch of
             '^' ->
               let cnt'    = cnt + 1
                   nextSet' = addCol (c - 1) (addCol (c + 1) nextSet)
               in (cnt', nextSet')
             _   ->
               (cnt, addCol c nextSet)

      go :: Int -> S.Set Col -> Int -> Int
      go r activeCols cnt
        | S.null activeCols = cnt
        | r >= h            = cnt
        | otherwise =
            let (cnt', nextCols) =
                  S.foldl' (processCell r) (cnt, S.empty) activeCols
            in go (r + 1) nextCols cnt'

  in go startR (S.singleton startC) 0

solvePart1 :: String -> Int
solvePart1 = simulate . lines

main :: IO ()
main =
  getArgs >>= \case
    [] ->
      interact (show . solvePart1)
    [filePath] ->
      readFile filePath >>= print . solvePart1
    _ ->
      putStrLn "Usage: program [input-file]"
