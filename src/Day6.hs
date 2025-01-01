module Day6 where

import Data.Array.IArray qualified as A
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (..))
import Data.Containers.ListUtils (nubOrd)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (nub, sort)
import Data.Set qualified as Set
import MyLib (Direction (..), drawArray, toIndex)
import Paths_AOC2024 (getDataDir)

buildXYMap m = (xmap, ymap)
  where
    xmap =
      IM.fromListWith
        (<>)
        [ (x, IS.singleton y)
          | ((x, y), '#') <- A.assocs m
        ]
    ymap =
      IM.fromListWith
        (<>)
        [ (y, IS.singleton x)
          | ((x, y), '#') <- A.assocs m
        ]

randomDropXY m (xmap, ymap) =
  [ (xmap', ymap')
    | (x, y) <- m,
      let xmap' = IM.insertWith (<>) x (IS.singleton y) xmap,
      let ymap' = IM.insertWith (<>) y (IS.singleton x) ymap
  ]

skipWalk m@(xmap, ymap) d i@(x, y) = (i, d) : maybe [] (skipWalk m (succ d)) i'
  where
    i' = case d of
      North -> (x,) . (+ 1) . fst <$> IS.maxView n
      South -> (x,) . subtract 1 . fst <$> IS.minView s
      West -> (,y) . (+ 1) . fst <$> IS.maxView w
      East -> (,y) . subtract 1 . fst <$> IS.minView e
    (n, s) = maybe (IS.empty, IS.empty) (IS.partition (< y)) (xmap IM.!? x)
    (w, e) = maybe (IS.empty, IS.empty) (IS.partition (< x)) (ymap IM.!? y)

isLoop a = go a a
  where
    go (x : xs) (_ : y : ys) = x == y || go xs ys
    go _ _ = False

walk m i d =
  case m A.!? i' of
    Nothing -> [(i, d)]
    Just '#' -> walk m i (succ d)
    Just _ -> (i, d) : walk m i' d
  where
    i' = bimap (+ fst i) (+ snd i) (toIndex d)

dropObs m start d i = isLoop $ walk (m A.// [(i, '#')]) start d

day6 :: IO ()
day6 = do
  -- input <- readFile . (++ "/input/test6.txt") =<< getDataDir
  m <- drawArray @UArray . lines <$> (readFile . (++ "/input/input6.txt") =<< getDataDir)
  let start = head [i | (i, '^') <- A.assocs m]
      ansA = nubOrd $ map fst $ walk m start North
      ansB = length $ filter (dropObs m start North) $ tail ansA
      ansB' = length $ filter (\x -> isLoop $ skipWalk x North start) $ randomDropXY (tail ansA) $ buildXYMap m
  putStrLn ("day6a: " ++ show (length ansA))
  putStrLn ("day6b: " ++ show ansB')
