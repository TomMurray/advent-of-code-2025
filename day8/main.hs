module Main where
import Control.Monad.Cont (label)
import Data.Function (on)
import Data.List (sortBy)
import Data.Ord (Down(..))

newtype Vec3 e = Vec3 (e, e, e)
  deriving (Show, Eq, Ord)

splitOn :: Char -> String -> [String]
splitOn c s =
  let (before, after) = break (== c) s
  in before : if null after then [] else splitOn c (tail after)

parseVec3 :: Read e => String -> Vec3 e
parseVec3 s =
  let [x, y, z] = map read $ splitOn ',' s
  in Vec3 (x, y, z)

distanceSq :: Num a => Vec3 a -> Vec3 a -> a
distanceSq (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
  (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

newtype NodeId = NodeId Int deriving (Enum, Num, Show, Eq, Ord)
newtype WeightedEdge e = WeightedEdge (NodeId, NodeId, e) deriving (Show, Eq, Ord)

toWeightedEdges points = do
  let labeled = zip [0..] points
  v1 <- labeled
  v2 <- labeled
  let v1id = fst v1
  let v2id = fst v2
  if v1id /= v2id && v1id < v2id then
    return (WeightedEdge (v1id, v2id, distanceSq (snd v1) (snd v2)))
  else
    []

getWeight :: WeightedEdge e -> e
getWeight (WeightedEdge (_, _, w)) = w

-- Build an adjacency matrix for fast lookup?

main :: IO ()
main = do
  text <- readFile "input.txt"
  -- Work with squared distances to allow Integer arithmetic
  -- and avoid possible floating point issues
  let relays = map (parseVec3 :: String -> Vec3 Integer) $ lines text

  -- Generate pairwise (squared)distance between vectors
  let sorted = sortBy (compare `on` getWeight) $ toWeightedEdges relays
  print $ take 10 sorted