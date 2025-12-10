module Main where

data Vec2 a = Vec2 a a deriving (Show, Eq, Ord)
type IVec2 = Vec2 Int

parseCoords :: [Char] -> IVec2
parseCoords str =
  let (x, y) = break (== ',') str
  in Vec2 (read x) (read (tail y))

allAreas l = do
  (i1, Vec2 x1 y1) <- l
  (i2, Vec2 x2 y2) <- l
  let absWidth = abs (x1 - x2 + 1)
  let absHeight = abs (y1 - y2 + 1)
  if i1 < i2 then return (absWidth * absHeight) else []


main :: IO ()
main = do
  text <- readFile "input.txt"
  let coords = map parseCoords $ lines text

  print $ maximum $ allAreas $ zip [0..] coords
