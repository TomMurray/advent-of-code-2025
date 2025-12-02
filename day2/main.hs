
module Main where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter str =
    let (before, after) = break (== delimiter) str
    in before : if null after then [] else splitOn delimiter (tail after)

parseRange :: String -> (Integer, Integer)
parseRange s =
  let (before, after) = break (== '-') s
  in (read before, read $ tail after)

ceilLog10 :: Integral n => n -> n
ceilLog10 n = ceiling (logBase 10 (fromIntegral n))

-- Slow, dumb version
getInvalidCounterPart :: Integral p => p -> p
getInvalidCounterPart n =
  let digits = ceilLog10 n `div` 2
      multiplier = 10 ^ digits
      half = n `div` multiplier
      inv = half + half * multiplier
  in inv
isInvalid n =
  even (ceilLog10 n) && getInvalidCounterPart n == n

countInvalidIDs start end =
  sum $ filter isInvalid [start..end]

main :: IO ()
main = do
    text <- readFile "input.txt"
    -- Expect a single line input
    let l = head $ lines text

    -- Parse into ranges
    let ranges = map parseRange $ splitOn ',' l

    -- Part 1
    let invalidCounts = map (uncurry countInvalidIDs) ranges
    print (sum invalidCounts)