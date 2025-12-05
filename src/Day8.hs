module Day8 where

import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Paths_AOC2024 (getDataDir)

type Index = (Int, Int)

inputParser (x : xs) = ((l, h), go Map.empty 0 0 x xs)
 where
  l = length x
  h = length xs + 1
  go m x y [] [] = m
  go m x y [] (z : zs) = go m 0 (y + 1) z zs
  go m x y ('.' : us) zs = go m (x + 1) y us zs
  go m x y (u : us) zs = go (Map.insertWith (<>) u (Set.singleton (x, y)) m) (x + 1) y us zs

calcAntinode :: Bool -> (Index, Index) -> Set Index -> Set Index
calcAntinode flag ((minX, minY), (maxX, maxY)) s = Set.fromList do
  a@(ax, ay) <- s'
  b@(bx, by) <- s'
  guard $ a /= b
  let (x, y) = (ax - bx, ay - by)
      fa = bimap (+ x) (+ y)
      fb = bimap (subtract x) (subtract y)
      p (i, j) = i >= minX && i < maxX && j >= minY && j < maxY
      la = takeWhile p $ iterate fa a
      lb = takeWhile p $ iterate fb b
  if flag then la <> lb else take 1 (tail la) <> take 1 (tail lb)
 where
  s' = Set.toList s

solve b ((l, h), m) = Set.size . Set.unions . map (calcAntinode b ((0, 0), (l, h))) $ Map.elems m
solveA = solve False
solveB = solve True

day8 :: IO (String, String)
day8 = do
  -- input <- inputParser . lines <$> (readFile . (++ "/input/test8.txt") =<< getDataDir)
  input <- inputParser . lines <$> (readFile . (++ "/input/input8.txt") =<< getDataDir)
  let
    !finalAnsa =
      show $
        solveA input
  let
    !finalAnsb =
      show $
        solveB input
  pure (finalAnsa, finalAnsb)
