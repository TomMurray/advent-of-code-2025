{-# LANGUAGE BlockArguments #-}

dir :: Num a => String -> a
dir "L" = -1
dir "R" = 1

easywrap :: Int -> Int
easywrap a = ((a `mod` 100) + 100) `mod` 100

main :: IO ()
main = do
    text <- readFile "input.txt"
    -- Get full list of rotations
    let rotations = (map ((\(a, b) -> dir a * read b :: Int) . splitAt 1) . lines) text

    -- Start at 50
    let start = 50
    let cumPositions = scanl (\acc r -> easywrap (acc + r)) start rotations
    
    -- Count the number of zeroes
    print (length (filter (== 0) cumPositions))
