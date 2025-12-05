module Main where

main :: IO ()
main = do
  text <- readFile "input.txt"
  print $ lines text