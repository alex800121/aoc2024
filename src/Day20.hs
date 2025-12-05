module Day20 where

import Data.Array.IArray qualified as A
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List (find)
import Data.Set qualified as Set
import MyLib (drawArray, toIndex)
import Paths_AOC2024 (getDataDir)

type Index = (Int, Int)

solve shortCut target = go IM.empty 0 0
  where
    go acc n pathLen [] = n
    go !acc !n !pathLen ((x, y) : es) = go acc' n' pathLen' es
      where
        xpy = x + y
        xmy = x - y
        pathLen' = succ pathLen
        !acc' = IM.insertWith (<>) xpy (IM.singleton xmy ((x, y), pathLen)) acc
        !xs = snd $ IM.split (xpy - shortCut - 1) $ fst $ IM.split (xpy + shortCut + 1) acc
        !n' =
          n
            + sum
              ( IM.map
                  ( IM.size
                      . IM.filter (\((x0, y0), p0) -> (pathLen - p0) - (abs (x0 - x) + abs (y0 - y)) >= target)
                      . snd
                      . IM.split (xmy - shortCut - 1)
                      . fst
                      . IM.split (xmy + shortCut + 1)
                  )
                  xs
              )

readPath s = go Set.empty end
  where
    ss = drawArray $ lines s :: UArray Index Char
    start = head [p | (p, 'S') <- A.assocs ss]
    end = head [p | (p, 'E') <- A.assocs ss]
    go tra e@(x, y)
      | start == e = [e]
      | otherwise = e : go tra' e'
      where
        Just e' = find (\p -> p `Set.notMember` tra' && Just '#' /= ss A.!? p) $ map (bimap (+ x) (+ y) . toIndex) [minBound .. maxBound]
        tra' = Set.insert e tra

day20 :: IO (String, String)
day20 = do
  path <- readPath <$> (readFile . (++ "/input/input20.txt") =<< getDataDir)
  let !finalAnsa =
        show $
          solve 2 100 path
  let !finalAnsb =
        show $
          solve 20 100 path
  pure (finalAnsa, finalAnsb)
