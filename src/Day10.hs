module Day10 where

import Data.Array.Unboxed
import Data.Bifunctor (Bifunctor (..))
import Data.Char (digitToInt)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import MyLib (drawArray)
import Paths_AOC2024 (getDataDir)

type Index = (Int, Int)

type A = UArray Index Int

adjacent = [(0, 1), (0, -1), (-1, 0), (1, 0)]

bfs :: (Int -> Int -> Bool) -> (Int -> Bool) -> A -> Index -> Map Index Int
bfs ad fi a s = go Map.empty Set.empty (Map.singleton s 1)
  where
    go o travelled start
      -- | traceShow start False = undefined
      | Map.null start = o
      | otherwise = go o' travelled' start'
      where
        travelled' = travelled <> Map.keysSet start
        (o', start') = Map.foldlWithKey' f (o, Map.empty) start
        f (accO, accS) k@(kx, ky) n = (Map.unionWith (+) accO $ Map.fromList o, Map.unionWith (+) accS $ Map.fromList s)
          where
            (o, s) =
              bimap (map fst) (map fst) $
                partition
                  (fi . snd)
                  [ ((k', n), x')
                    | (ax, ay) <- adjacent,
                      let k' = (ax + kx, ay + ky),
                      x' <- maybeToList (a !? k'),
                      ad (a ! k) x',
                      Set.notMember k' travelled'
                  ]

solveA a =
  [ (k, bfs (\a b -> succ a == b) (== 9) a k)
    | (k, i) <- assocs a,
      i == 0
  ]

day10 :: IO ()
day10 = do
  -- input <- drawArray @UArray . map (map digitToInt) . lines <$> (readFile . (++ "/input/test10.txt") =<< getDataDir)
  input <- drawArray @UArray . map (map digitToInt) . lines <$> (readFile . (++ "/input/input10.txt") =<< getDataDir)
  putStrLn
    . ("day10a: " ++)
    . show
    . sum
    . map (Map.size . snd)
    $ solveA input
  putStrLn
    . ("day10b: " ++)
    . show
    . sum
    . map (sum . snd)
    $ solveA input
