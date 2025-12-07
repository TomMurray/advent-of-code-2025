module Main where

import Data.List (tails, elemIndex)

windows :: Int -> [a] -> [[a]]
windows n = map (take n) . takeWhile ((>=n) . length) . tails

pad1D :: Num a => Int -> [a] -> [a]
pad1D n l =
  replicate n 0 ++ l ++ replicate n 0

conv1D kernel =
  let kernelWidth = length kernel
  in map (sum . zipWith (*) kernel) .windows kernelWidth . pad1D (kernelWidth `div` 2)

isSplitter '^' = 1
isSplitter _ = 0

isStart 'S' = 1
isStart _ = 0

part1Row (count, beams) row =
  let intersections = zipWith (*) beams (map isSplitter row)
      splitCount = sum intersections
      maskedBeams = zipWith (*) beams (map (1-) intersections)
      splitBeams = map (min 1) $ conv1D [1, 0, 1] intersections
      nextBeams = map (min 1) $ zipWith (+) maskedBeams splitBeams
  in (count + splitCount, nextBeams)

part2Row beams row =
  let intersected = zipWith (*) beams (map isSplitter row)
      maskedBeams = zipWith (*) beams (map ((1-) . min 1) intersected)
      splitBeams = conv1D [1, 0, 1] intersected
      nextBeams = zipWith (+) maskedBeams splitBeams
  in nextBeams


main :: IO ()
main = do
  text <- readFile "input.txt"
  let rows = lines text

  -- State for the algorithm is a count (the answer)
  -- and the current row of active beams (True or False).
  -- I'm going to use a dense list for simplicity.

  -- Assume the first row must have an 'S' in it
  let beams = map isStart (head rows)

  -- One iteration checks each position in the current beams list
  -- with splitters in the next row in the grid:
  --  * First compute beam/splitter intersections
  --  * Then update beams list by removing the beam at intersected
  --    positions and setting beams in neighbouring positions.
  let result = foldl part1Row (0, beams) (tail rows)
  putStrLn $ "Part 1: " ++ show (fst result)

  -- Part 2 many worlds
  -- I think this is similar but without limiting to 1 beam per position
  -- and instead summing the number of beams in each position.
  let result = sum $ foldl part2Row beams (tail rows)
  putStrLn $ "Part 2: " ++ show result
