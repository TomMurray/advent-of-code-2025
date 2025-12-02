
module Main where

-- Assuming max digits is 15
divisor 1 = 111111111111111
divisor 2 = 101010101010101
divisor 3 = 001001001001001
divisor 4 = 001000100010001
divisor 5 = 000010000100001

testDivisor l digits = divisor l `mod` (10 ^ digits)

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

isInvalid n =
  let digits = ceilLog10 n
  in even digits && n `mod` testDivisor (digits `div` 2) digits == 0

isInvalidPart2 n =
  let digits = ceilLog10 n
      tests = filter (\n -> digits `mod` n == 0) [1..digits `div` 2]
  in any (\t -> n `mod` testDivisor t digits == 0) tests

countInvalidIDs start end =
  sum $ filter isInvalid [start..end]

countInvalidIDsPart2 start end =
  sum $ filter isInvalidPart2 [start..end]

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

    -- Part 2
    print $ sum $ map (uncurry countInvalidIDsPart2) ranges