module Day18 where

import Data.Bifunctor (Bifunctor (..))
import Data.List (partition)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.PQueue.Prio.Min (MinPQueue (..))
import Data.PQueue.Prio.Min qualified as Q
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import MyLib (Direction (..), drawGraph, toIndex)
import Paths_AOC2024 (getDataDir)

type M = Set Index

type Index = (Int, Int)

type MPlus = Map Index [(Int, (Direction, Index))]

floodOnce :: [Index] -> M -> Index -> [(Int, (Direction, Index))]
floodOnce int m i = go [] 1 is
  where
    is = filter ((`Set.member` m) . snd) $ map ((,) <$> id <*> bimap (+ fst i) (+ snd i) . toIndex) [minBound .. maxBound]
    go acc n start
      | null start = acc
      | otherwise = go acc' (n + 1) start''
      where
        f (d, i) = filter ((`Set.member` m) . snd) [(d', i') | d' <- [pred d, succ d], let i' = bimap (+ fst i) (+ snd i) (toIndex d')]
        (intersections, start') = partition ((||) <$> (`elem` int) . snd <*> not . null . f) start
        acc' = acc <> map (n,) intersections
        start'' = filter ((`Set.member` m) . snd) $ map (\(d, i) -> (d, bimap (+ fst i) (+ snd i) (toIndex d))) start'

flood :: [Index] -> M -> Index -> MPlus
flood int m i = go Map.empty Set.empty [i]
  where
    go acc _ [] = acc
    go acc travelled (x : xs)
      | x `Set.member` travelled = go acc travelled xs
      | otherwise = go acc' travelled' (xs' <> xs)
      where
        next = floodOnce int m x
        acc' = Map.insertWith (<>) x next acc
        travelled' = Set.insert x travelled
        xs' = map (snd . snd) next

aStar :: MPlus -> Index -> Index -> Maybe Int
aStar m end start = go Set.empty (Q.singleton (calc start) (0, start))
  where
    calc (x, y) = abs (x - fst end) + abs (y - snd end)
    go _ Empty = Nothing
    go travelled ((h, (n, start)) :< q)
      | start == end = Just n
      | start `Set.member` travelled = go travelled q
      | otherwise = go travelled' q'
      where
        travelled' = Set.insert start travelled
        start' =
          map (\x -> (calc (snd $ snd x) + n + fst x, bimap (+ n) snd x))
            . filter ((`Set.notMember` travelled') . snd . snd)
            $ m Map.! start
        q' = q <> Q.fromList start'

binSearch input ((minx, maxx), (miny, maxy)) end start = go 0 (length input)
  where
    go lb hb
      | lb == mid = Just mid
      | isJust (aStar mplus end start) = go mid hb
      | otherwise = go lb mid
      where
        mid = (lb + hb) `div` 2
        mplus = build input ((minx, maxx), (miny, maxy)) start end mid

build input ((minx, maxx), (miny, maxy)) start end n = flood [start, end] m start
  where
    m = Set.fromList [(x, y) | x <- [minx .. maxx], y <- [miny .. maxy]] Set.\\ Set.fromList (take n input)

day18 :: IO (String, String)
day18 = do
  input <- map ((\[x, y] -> (read @Int x, read @Int y)) . splitOn ",") . lines <$> (readFile . (++ "/input/input18.txt") =<< getDataDir)
  let mplus = build input ((0, 70), (0, 70)) (70, 70) (0, 0) 1024
  let !finalAnsa =
        show $
          aStar mplus (70, 70) (0, 0)
  let !finalAnsb =
        show $
          (input !!) <$> binSearch input ((0, 70), (0, 70)) (70, 70) (0, 0)
  pure (finalAnsa, finalAnsb)
