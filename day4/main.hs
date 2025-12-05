module Main where

import Data.List (tails)
import Control.Monad (liftM2)

mapInputChar :: Char -> Int
mapInputChar c =
  case c of '.' -> 0
            '@' -> 1

windows :: Int -> [a] -> [[a]]
windows n = map (take n) . takeWhile ((>=n) . length) . tails

pad1D :: Num a => Int -> [a] -> [a]
pad1D n l =
  replicate n 0 ++ l ++ replicate n 0

conv1D :: Num b => Int -> [b] -> [b]
conv1D n = map sum . windows n . pad1D (n `div` 2)

boolToInt :: Num a => Bool -> a
boolToInt x = if x then 1 else 0

conv2D :: Num a => Int -> [[a]] -> [[a]]
conv2D n l =
  let width = length (head l)
      padAmount = n `div` 2 -- Intentionally rounded down
      rowPadding = replicate padAmount $ replicate width 0
  in map (foldl (zipWith (+)) (replicate width 0)) $ windows 3 $ map (conv1D n) $ rowPadding ++ l ++ rowPadding

findRemovable m = do
  let surroundCount = conv2D 3 m
  let masked = zipWith (zipWith (*)) surroundCount m
  let removableRolls = map (map (boolToInt . liftM2 (&&) (> 0) (<= 4))) masked
  removableRolls

popcount :: [[Int]] -> Int
popcount m =
  sum $ map (length . filter (== 1)) m

main :: IO ()
main = do
  text <- readFile "example.txt"

  -- Map input lines to [[Int]]
  let rolls = map (map mapInputChar) $ lines text

  let removableRolls = findRemovable rolls
  let count = popcount removableRolls

  -- Part 1
  putStrLn $ "Part 1: " ++ show count

  -- Part 2
  -- Iterate until a fixed point (no more rolls can be removed) and
  -- give the total number of rolls removed.

  
