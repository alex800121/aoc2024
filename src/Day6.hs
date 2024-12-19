module Day6 where

import Data.Array.IArray qualified as A
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (..))
import Data.Containers.ListUtils (nubOrd)
import Data.List (nub, sort)
import Data.Set qualified as Set
import MyLib (Direction (..), drawArray, toIndex)
import Paths_AOC2024 (getDataDir)

type Index = (Int, Int)

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
  putStrLn ("day6a: " ++ show (length ansA))
  putStrLn ("day6b: " ++ show ansB)
