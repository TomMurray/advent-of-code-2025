module Main where

import Data.List (tails)

windows :: Int -> [a] -> [[a]]
windows n = map (take n) . takeWhile ((>=n) . length) . tails

pad1D :: Int -> [Bool] -> [Bool]
pad1D n l =
  replicate n False ++ l ++ replicate n False

conv1D :: [Bool] -> [Bool] -> [Bool]
conv1D kernel =
  let kernelWidth = length kernel
  in map (or . zipWith (&&) kernel) . windows kernelWidth . pad1D (kernelWidth `div` 2)



isSplitter c = c == '^'

computeRow (count, beams) row =
  let intersections = zipWith (&&) beams (map isSplitter row)
      splitBeams = conv1D [True, False, True] intersections
      splitCount = length $ filter id intersections
      maskedBeams = zipWith (&&) beams (map not intersections)
      nextBeams = zipWith (||) maskedBeams splitBeams
  in (count + splitCount, nextBeams)

main :: IO ()
main = do
  text <- readFile "input.txt"
  let rows = lines text

  -- State for the algorithm is a count (the answer)
  -- and the current row of active beams (True or False).
  -- I'm going to use a dense list for simplicity.
  
  -- Assume the first row must have an 'S' in it
  let beams = map (== 'S') (head rows)

  -- One iteration checks each position in the current beams list
  -- with splitters in the next row in the grid:
  --  * First compute beam/splitter intersections
  --  * Then update beams list by removing the beam at intersected
  --    positions and setting beams in neighbouring positions.
  let result = foldl computeRow (0, beams) (tail rows)
  putStrLn $ "Part 1: " ++ show (fst result)
  