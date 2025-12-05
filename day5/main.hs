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


-- Like a binary search & split, specifically for
-- intervals, splitting before the interval that contains x
splitBefore [] x = ([], [])
splitBefore [(begin, end)] x
  | x <= end + 1 = ([], [(begin, end)])
  | otherwise = ([(begin, end)],[])

-- Result is 2 lists of intervals
splitBefore iset x =
  let (left, right) = splitAt (length iset `div` 2) iset
      splitLeft = (not (null left) && (x <= snd (last left) + 1))
      (ll, lr) = if splitLeft then splitBefore left x else (left, [])
      (rl, rr) = if splitLeft then (right, []) else splitBefore right x
  in if splitLeft then (ll, lr ++ rl) else (ll ++ rl, rr)

splitAfter [] x = ([], [])
splitAfter [(begin, end)] x
  | x >= begin - 1 = ([(begin, end)], [])
  | otherwise = ([], [(begin, end)])

splitAfter iset x =
  let (left, right) = splitAt (length iset `div` 2) iset
      splitRight = (not (null right) && (x >= fst (head right) - 1))
      (ll, lr) = if splitRight then (left, []) else splitAfter left x
      (rl, rr) = if splitRight then splitAfter right x else (right, [])
  in if splitRight then (ll ++ rl, rr) else (ll, lr ++ rl)

-- Build an interval set by insertion
writeInterval [] i = [i]
writeInterval iset (begin, end) =
  let (b0, e0) = head iset
      (bn, en) = last iset
  in [(min begin b0, max end en)]

-- Base case: empty set becomes the intervalinsertInterval [] i = [i]
insertInterval [] i = [i]
insertInterval iset (begin, end) =
  let e0 = snd (head iset)
      bn = fst (last iset)
  in if begin <= e0 && end >= bn then [(min begin e0, max end bn)]
  else
    let (lhs, x) = splitBefore iset begin
        (candidate, rhs) = splitAfter x end
    in lhs ++ writeInterval candidate (begin, end) ++ rhs


inInterval :: Ord a => (a, a) -> a -> Bool
inInterval (begin, end) x = begin <= x && x <= end

contains [] _ = False
contains [i] x = inInterval i x
contains iset x =
  let (left, right) = splitAt (length iset `div` 2) iset
      searchRight = (not (null right) && (x >= fst (head right)))
    in if searchRight then contains right x else contains left x

main :: IO ()
main = do
  text <- readFile "input.txt"
  let (fresh, available) = splitOnce "" $ lines text

  -- Parse intervals
  let freshIntervals = map parseInterval fresh
  let availableItems = map read available :: [Integer]


  -- Construct an interval set of the intervals
  let intervalSet = foldl insertInterval [] freshIntervals

  -- Part 1
  let availableAndFresh = length $ filter (contains intervalSet) availableItems
  putStrLn $ "Part 1: " ++ show availableAndFresh

  -- Part 2
  -- Just sum up the length of each interval in the set
  let totalFresh = sum $ map (\(begin, end) -> end - begin + 1) intervalSet
  putStrLn $ "Part 2: " ++ show totalFresh