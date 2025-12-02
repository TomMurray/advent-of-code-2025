{-# LANGUAGE BlockArguments #-}

dir :: Num a => String -> a
dir "L" = -1
dir "R" = 1

enumToInt :: Bool -> Int
enumToInt False = 0
enumToInt True  = 1

-- We maintain a range of -99 -> 99
-- We first add the rotation and see if we ticked through zero
count :: Int -> Int -> (Int, Int)
count n r =
    let n' = n + r
        crossed = abs (signum n' - signum n) == 2
        exact = n' == 0
        wrapcount = abs n' `div` 100
        totalcount = wrapcount + enumToInt crossed + enumToInt exact
    in (totalcount, n' - wrapcount * 100 * signum n')

wrap :: Int -> Int
wrap a = ((a `mod` 100) + 100) `mod` 100

main :: IO ()
main = do
    text <- readFile "input.txt"
    -- Get full list of rotations
    let rotations = (map ((\(a, b) -> dir a * read b :: Int) . splitAt 1) . lines) text

    -- Start at 50
    let start = 50

    -- Part 1
    let positions = scanl (\acc r -> wrap (acc + r)) start rotations
    print (length (filter (== 0) positions))

    -- Part 2
    let zerocounts = map fst $ scanl (\(_, n) r -> count n r) (0, start) rotations
    print (sum zerocounts)
