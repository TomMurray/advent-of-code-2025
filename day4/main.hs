module Main where

import Data.List (tails)
import Distribution.Simple.Setup (replOptions)

mapInputChar :: Char -> Int
mapInputChar c =
  case c of '.' -> 0
            '@' -> 1

windows :: Int -> [a] -> [[a]]
windows n = map (take n) . takeWhile ((>=n) . length) . tails

pad1D :: Num a => Int -> [a] -> [a]
pad1D n l =
  replicate n 0 ++ l ++ replicate n 0

pad2D :: Num a => Int -> [[a]] -> [[a]]
pad2D n l =
  let width = length (head l)
      height = length l
      outerPadding = replicate n $ replicate (width + n * 2) 0
  in outerPadding ++ map (pad1D n) l ++ outerPadding

conv1D n = map sum . windows n . pad1D (n `div` 2)

conv2D :: Num a => Int -> [[a]] -> [[a]]
conv2D n l =
  let width = length (head l)
      padAmount = n `div` 2 -- Intentionally rounded down
      rowPadding = replicate padAmount $ replicate width 0
  in map (foldl (zipWith (+)) (replicate width 0)) $ windows 3 $ map (conv1D n) $ rowPadding ++ l ++ rowPadding

main :: IO ()
main = do
  text <- readFile "example.txt"

  -- Map input lines to [[Int]]
  let rolls = map (map mapInputChar) $ lines text

  -- Now work out which rolls are surrounded by < 4 other rolls via sum pooling
  print $ conv2D 3 rolls