module Main where

splitOnce :: Eq a => a -> [a] -> ([a], [a])
splitOnce delimiter l =
  let (before, after) = break (== delimiter) l
  in (before, tail after)

type Interval = (Integer, Integer)

parseInterval :: String -> Interval
parseInterval s =
  let (begin, end) = break (== '-') s
  in (read begin, read $ tail end)

main :: IO ()
main = do
  text <- readFile "input.txt"
  let (fresh, available) = splitOnce "" $ lines text
    
  -- Parse intervals
  let freshIntervals = map parseInterval fresh
  let availableItems = map read available :: [Integer]
  print freshIntervals
  print availableItems
