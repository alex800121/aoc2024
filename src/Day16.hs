module Day16 where

import Control.Monad.Hefty
import Control.Monad.Hefty.State (State, modify, runState)
import Data.Bifunctor (Bifunctor (..))
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.PQueue.Prio.Min (MinPQueue (..))
import Data.PQueue.Prio.Min qualified as Q
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import MyLib
  ( Direction (..),
    drawArray,
    drawGraph,
    drawMap,
    drawMapWithKeyM,
    toIndex,
  )
import Paths_AOC2024 (getDataDir)

type M = Set Index

type MPlus = Map Index [(Int, (Direction, Index))]

type Index = (Int, Int)

dijkstra :: (Index -> Bool) -> MPlus -> (Direction, Index) -> [(Int, [Index])]
dijkstra end mplus start = go Map.empty maxBound (Q.singleton 0 [start])
  where
    go _ _ Empty = []
    go travelled max ((n, r@((d, i) : rest)) :< q)
      | max < n = []
      | end i = (n, map snd r) : go travelled' n q
      | maybe False (< n) (travelled Map.!? (d, i)) = go travelled max q
      | otherwise = go travelled' max q'
      where
        travelled' = Map.insertWith min (d, i) n travelled
        q' = q <> Q.fromList (map (second (: r)) l)
        l = mapMaybe f (mplus Map.! i)
        f (n', (d', i'))
          | d' `elem` [succ d, pred d] = Just (n' + n + 1000, (d', i'))
          | d' == succ (succ d) = Nothing
          | d' == d = Just (n' + n, (d', i'))

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

readInput :: String -> ((Index, Index), M)
readInput s = second Map.keysSet $ runPure $ runState ((0, 0), (0, 0)) $ drawMapWithKeyM f (0, 0) s
  where
    f (_, y) '\n' = pure ((0, y + 1), Nothing)
    f (x, y) '.' = pure ((x + 1, y), Just ())
    f (x, y) 'S' = modify @(Index, Index) (first (const (x, y))) >> pure ((x + 1, y), Just ())
    f (x, y) 'E' = modify @(Index, Index) (second (const (x, y))) >> pure ((x + 1, y), Just ())
    f (x, y) _ = pure ((x + 1, y), Nothing)

buildPoints :: [Index] -> [Index]
buildPoints [] = []
buildPoints [x] = [x]
buildPoints ((x0, y0) : x@(x1, y1) : xs) = [(x, y) | x <- [xmin .. xmax], y <- [ymin .. ymax]] <> buildPoints (x : xs)
  where
    xmin = min x0 x1
    xmax = max x0 x1
    ymin = min y0 y1
    ymax = max y0 y1

day16 :: IO ()
day16 = do
  ((start, end), input) <- readInput <$> (readFile . (++ "/input/input16.txt") =<< getDataDir)
  -- ((start, end), input) <- readInput <$> (readFile . (++ "/input/test16.txt") =<< getDataDir)
  -- ((start, end), input) <- readInput <$> (readFile . (++ "/input/test16'.txt") =<< getDataDir)
  let mplus = flood [start, end] input start
      ans = dijkstra (== end) mplus (East, start)
  putStrLn
    . ("day16a: " ++)
    . show
    . fmap fst
    $ listToMaybe ans
  putStrLn
    . ("day16b: " ++)
    . show
    . Set.size
    . Set.fromList
    $ concatMap (buildPoints . snd) ans
