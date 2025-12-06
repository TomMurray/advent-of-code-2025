module Main where
import Control.Monad (join)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose lists
  | length (head lists) == 1 = [join lists]
  | otherwise = map head lists:transpose (map tail lists)

getFoldOp "+" = (+)
getFoldOp "*" = (*)

compute problem =
  let foldOp = getFoldOp (last problem)
  in foldr1 foldOp (map read $ init problem)

main :: IO ()
main = do
  text <- readFile "input.txt"

  let grid = map words $ lines text
  let problems = transpose grid

  -- Compute
  let solutions = map compute problems

  -- Part 1
  putStrLn $ "Part 1: " ++ show (sum solutions)