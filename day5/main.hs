module Main where
import Distribution.Simple.Utils (xargs)

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
splitBefore [(begin, end)] x
  | x <= end + 1 = ([], [(begin, end)])
  | otherwise = ([(begin, end)],[])

-- Result is 2 lists of intervals
splitBefore iset x =
  let (left, right) = splitAt (length iset `div` 2) iset
      lhsEnd = if null left then minBound else snd (last left)
      (ll, lr) = if x <= lhsEnd + 1 then splitBefore left x else (left, [])
      (rl, rr) = if x <= lhsEnd + 1 then (right, []) else splitBefore right x
  in if x <= lhsEnd + 1 then (ll, lr ++ rl) else (ll ++ rl, rr)

splitAfter [(begin, end)] x
  | x >= begin - 1 = ([(begin, end)], [])
  | otherwise = ([], [(begin, end)])

splitAfter iset x =
  let (left, right) = splitAt (length iset `div` 2) iset
      rhsBegin = if null right then maxBound else fst (head right)
      (ll, lr) = if x >= rhsBegin - 1 then (left, []) else splitAfter left x
      (rl, rr) = if x >= rhsBegin - 1 then splitAfter right x else (right, [])
  in if x >= rhsBegin - 1 then (ll ++ rl, rr) else (ll, lr ++ rl)

-- Build an interval set by insertion
writeInterval iset (begin, end) =
  let (b0, e0) = head iset
      (bn, en) = last iset
  in [(min begin b0, max end en)]

insertInterval :: (Num a, Bounded a, Ord a) => [(a, a)] -> (a, a) -> [(a, a)]
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
      rhsBegin = if null right then maxBound else fst (head right)
    in if x < rhsBegin then contains left x else contains right x


main :: IO ()
main = do
  text <- readFile "example.txt"
  let (fresh, available) = splitOnce "" $ lines text

  -- Parse intervals
  let freshIntervals = map parseInterval fresh
  let availableItems = map read available :: [Integer]
  print freshIntervals
  print availableItems

  -- TODO: Construct an interval set
  --       Basically binary search insertion
  --       Also binary search lookup
