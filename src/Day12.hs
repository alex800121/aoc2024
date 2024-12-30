module Day12 where

import Data.Array.Unboxed (Array)
import Data.Array.Unboxed qualified as UA
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import MyLib (drawArray, drawGraph, drawMap)
import Paths_AOC2024 (getDataDir)

type Index = (Int, Int)

type Garden = Map Index Char

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

bfs :: (Index -> [Index]) -> Garden -> Index -> Map Index Int
bfs next garden s = go Map.empty (Map.singleton s (garden Map.! s))
  where
    go accM start
      | Map.null start = accM
      | otherwise = uncurry go $ Map.foldlWithKey' f (accM, Map.empty) start
      where
        f (accM, start) k0 c0 = (accM', start')
          where
            ks = filter (\k -> Just c0 == garden Map.!? k) $ next k0
            accM' = Map.insert k0 (length ks) accM
            start' = start <> Map.fromList (mapMaybe (liftA2 (,) <$> pure <*> (garden Map.!?)) $ filter (`Map.notMember` accM') ks)

regionA :: Garden -> [Map Index Int]
regionA g
  | Map.null g = []
  | otherwise = ans : regionA g'
  where
    (k, _) = Map.findMin g
    ans = bfs (\(x, y) -> map (bimap (+ x) (+ y)) adjacent) g k
    g' = Map.filterWithKey (\k _ -> k `Map.notMember` ans) g

expand :: Set Index -> Set Index
expand s = Set.unions $ Set.map (\(x, y) -> let x2 = x * 2; y2 = y * 2 in Set.fromList [(x2, y2), (x2, y2 + 1), (x2 + 1, y2), (x2 + 1, y2 + 1)]) s

surround = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

detectCorners s = Set.filter (\(x, y) -> length (filter (\(a, b) -> (x + a, y + b) `Set.member` s) surround) `elem` [3, 4, 7]) s

day12 :: IO ()
day12 = do
  input <- drawMap Just . lines <$> (readFile . (++ "/input/input12.txt") =<< getDataDir)
  -- input <- drawMap Just . lines <$> (readFile . (++ "/input/test12.txt") =<< getDataDir)
  let regions = regionA input
  putStrLn
    . ("day12a: " ++)
    . show
    . sum
    $ map (\x -> Map.size x * (Map.size x * 4 - sum x)) regions
  putStrLn
    . ("day12b: " ++)
    . show
    . sum
    $ map ((*) <$> Map.size <*> Set.size . detectCorners . expand . Map.keysSet) regions
