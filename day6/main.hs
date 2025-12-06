module Main where
import Control.Monad (join)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose lists
  | length (head lists) == 1 = [join lists]
  | otherwise = map head lists:transpose (map tail lists)

padTo :: Int -> [Char] -> [Char]
padTo digits str =
  str ++ replicate (digits - length str) ' '

getFoldOp "+" = (+)
getFoldOp "*" = (*)

compute problem =
  let foldOp = getFoldOp (last problem)
  in foldr1 foldOp (map read $ init problem)

computePart2 problem =
  let foldOp = getFoldOp [last (head problem)]
  in foldr1 foldOp $ map (read . init) problem

splitBy f [] = []
splitBy f (x:xs) =
  let (left, right) = break f xs
  in (x:left) : if null right then [] else splitBy f (tail right)

main :: IO ()
main = do
  text <- readFile "input.txt"

  let grid = map words $ lines text
  let problems = transpose grid

  -- Compute
  let solutions = map compute problems

  -- Part 1
  putStrLn $ "Part 1: " ++ show (sum solutions)

  -- Part 2
  let transposed = transpose $ lines text
  -- Split by empty columns
  let problems = splitBy (all (== ' ')) transposed
  let solutions = map computePart2 problems
  putStrLn $ "Part 2: " ++ show (sum solutions)