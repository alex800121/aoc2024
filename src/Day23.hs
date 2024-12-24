module Day23 where

import Data.Bifunctor (Bifunctor (..))
import Data.Char (chr, ord)
import Data.Foldable1 (foldl1')
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (intercalate, maximumBy, sort, unfoldr)
import Data.List.Split (divvy, splitOn)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import Paths_AOC2024 (getDataDir)

toInt :: String -> Int
toInt = foldr (\x acc -> acc * 128 + ord x) 0

toChr :: Int -> String
toChr = unfoldr (\x -> if x <= 0 then Nothing else Just (chr (x `mod` 128), x `div` 128))

readInput = IM.fromListWith (<>) . concatMap (\x -> let [a, b] = map toInt $ splitOn "-" x in [(a, IS.singleton b), (b, IS.singleton a)]) . lines

solveA :: IntMap IntSet -> Set IntSet
solveA m = IM.foldlWithKey' f Set.empty m
  where
    f :: Set IntSet -> Int -> IntSet -> Set IntSet
    f acc0 k0 es0 = IS.foldl' g acc0 es0
      where
        g :: Set IntSet -> Int -> Set IntSet
        g acc e = es
          where
            es1 = m IM.! e
            es = IS.foldl' (\acc x -> Set.insert (IS.fromList [k0, e, x]) acc) acc $ es0 `IS.intersection` es1

-- solveB :: IntMap IntSet -> IntSet
-- solveB m = go (Set.fromList $ map IS.singleton $ IS.toList ks)
--   where
--     ks = IM.keysSet m
--     go :: Set IntSet -> IntSet
--     go start
--       | Set.null start' = Set.findMin start
--       | otherwise = go start'
--       where
--         intersections = IS.foldl' (\a -> IS.intersection a . (m IM.!)) ks
--         f s = IS.foldl' (\acc x -> Set.insert (IS.insert x s) acc) Set.empty (intersections s)
--         start' = Set.unions (Set.map f start)

bka :: IntMap IntSet -> IntSet
bka m = go IS.empty IS.empty (IM.keysSet m) IS.empty
  where
    c a b = if IS.size a > IS.size b then a else b
    go acc r p x
      | IS.null p && IS.null x = c acc r
      | IS.null p = acc
      | otherwise = acc'
      where
        (acc', _, _) = IS.foldl' f (acc, p, x) p
        f (acc, p, x) v = (acc', p', x')
          where
            neighbors = m IM.! v
            acc' = go acc (IS.insert v r) (IS.intersection p neighbors) (IS.intersection x neighbors)
            p' = IS.delete v p
            x' = IS.insert v x

day23 :: IO ()
day23 = do
  input <- readInput <$> (readFile . (++ "/input/input23.txt") =<< getDataDir)
  putStrLn
    . ("day23a: " ++)
    . show
    . length
    . Set.filter (any ((== 't') . head))
    . Set.map (map toChr . IS.toList)
    $ solveA input
  -- putStrLn
  --   . ("day23b: " ++)
  --   . intercalate ","
  --   . sort
  --   . map toChr
  --   . IS.toList
  --   $ solveB input
  putStrLn
    . ("day23b: " ++)
    . intercalate ","
    . sort
    . map toChr
    . IS.toList
    $ bka input
