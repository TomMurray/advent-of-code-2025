import Data.Char (digitToInt)

-- argmax which also returns the value
argmax :: (Num b, Ord a) => [a] -> (a, b)
argmax (x:xs) =
  case xs of
    [] -> (x, 0)
    _ ->
      let (maxs, idxs) = argmax xs in
      -- N.B. >= to ensure we get the earliest instance of the max value
      if x >= maxs then (x, 0) else (maxs, idxs + 1)

maxJoltage :: (Num a, Ord a) => [a] -> Int -> a
maxJoltage _ 0 = 0
maxJoltage l digits =
  let (val, idx) = argmax (take (length l - digits + 1) l)
  in val * 10 ^ (digits - 1) + maxJoltage (drop (idx + 1) l) (digits - 1)

main :: IO ()
main = do
  text <- readFile "input.txt"
  let banks = map (map digitToInt) (lines text)

  let maxJoltages = map (flip maxJoltage 2) banks

  putStrLn $ "Part 1: " ++ show (sum $ map (`maxJoltage` 2) banks)

  putStrLn $ "Part 2: " ++ show (sum $ map (`maxJoltage` 12) banks)



