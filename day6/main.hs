module Main where
import Control.Monad (join)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose lists
  | length (head lists) == 1 = [join lists]
  | otherwise = map head lists:transpose (map tail lists)

main :: IO ()
main = do
  text <- readFile "input.txt"

  let grid = map words $ lines text
  let problems = transpose grid
  
  print $ problems