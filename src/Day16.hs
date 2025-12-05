module Day16 where

import Data.Array.IArray qualified as A
import Data.Bifunctor (Bifunctor (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (foldl', partition)
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.PQueue.Prio.Min (MinPQueue (..))
import Data.PQueue.Prio.Min qualified as Q
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import MyLib (
  Direction (..),
  drawArray,
  drawGraph,
  toIndex,
 )
import Paths_AOC2024 (getDataDir)

type M = Set Index

type MPlus = IntMap [(Int, (Direction, Int))]

type Index = (Int, Int)

dijkstra :: Int -> Int -> MPlus -> (Direction, Int) -> Maybe (Int, Set Index)
dijkstra maxx end m start = go IS.empty IM.empty maxBound (Q.singleton 0 [start])
 where
  go _ _ _ Empty = Nothing
  go acc travelled !max ((n, r@(d, i) : rest) :< q)
    | max < n = Just (max, Set.fromList $ concatMap facc $ IS.toList acc)
    | end == i = go (IS.insert di acc) travelled' n q
    | Just (n', xs) <- travelled IM.!? di = if n' == n then go acc travelled' max q else go acc travelled max q
    | otherwise = go acc travelled' max q'
   where
    fdi (d, i) = fromEnum d + 4 * i
    !di = fdi r
    facc e =
      [ x
      | e' <- maybe [] snd (travelled IM.!? e)
      , x <- buildPoints maxx (e `div` 4) (e' `div` 4) <> facc e'
      ]
    travelled' = IM.insertWith (\(x, y) -> bimap (`min` x) (<> y)) di (n, map fdi $ take 1 rest) travelled
    !q' = foldl' f q ds
    !ds = m IM.! i
    f !q (n', (d', i'))
      | d' `elem` [succ d, pred d] = Q.insert (n' + n + 1000) r' q
      | d' == succ (succ d) = q
      | d' == d = Q.insert (n' + n) r' q
     where
      r' = [(d', i'), r]

toInt maxx (x, y) = x * maxx + y

flood :: Int -> [Index] -> M -> Index -> MPlus
flood maxx int m i = go IM.empty Set.empty [i]
 where
  go acc _ [] = acc
  go acc travelled (x : xs)
    | x `Set.member` travelled = go acc travelled xs
    | otherwise = go acc' travelled' (xs' <> xs)
   where
    next = floodOnce int m x
    acc' = IM.insertWith (<>) (toInt maxx x) (map (second (second (toInt maxx))) next) acc
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

readInput :: String -> (Int, (Index, Index), M)
readInput s = (maxx, (start, end), m)
 where
  a = drawArray @A.Array (lines s)
  start = head [x | (x, 'S') <- A.assocs a]
  end = head [x | (x, 'E') <- A.assocs a]
  maxx = snd $ snd $ A.bounds a
  m = Set.fromList [x | (x, e) <- A.assocs a, e /= '#']

fromInt maxx x = x `divMod` maxx

buildPoints :: Int -> Int -> Int -> [Index]
buildPoints maxx xy0 xy1 = [(x, y) | x <- [xmin .. xmax], y <- [ymin .. ymax]]
 where
  (x0, y0) = fromInt maxx xy0
  (x1, y1) = fromInt maxx xy1
  xmin = min x0 x1
  xmax = max x0 x1
  ymin = min y0 y1
  ymax = max y0 y1

day16 :: IO (String, String)
day16 = do
  (maxx, (start, end), input) <- readInput <$> (readFile . (++ "/input/input16.txt") =<< getDataDir)
  let m = flood maxx [start, end] input start
      ansA = dijkstra maxx (toInt maxx end) m (East, toInt maxx start)
  let
    !finalAnsa =
      show
        . fmap fst
        $ ansA
  let
    !finalAnsb =
      show $
        fmap
          ( Set.size
              . snd
          )
          ansA
  pure (finalAnsa, finalAnsb)
