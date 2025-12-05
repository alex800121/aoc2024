module Day7 where

-- import Control.Monad.Hefty
-- import Control.Monad.Hefty.NonDet

import Control.Applicative
import Control.Monad (unless)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Functor (($>))
import Data.List (partition)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Paths_AOC2024 (getDataDir)
import Text.Read (readMaybe)

inputParser :: String -> Maybe (Int, [Int])
inputParser s = do
  let [a, b] = splitOn ": " s
  (,) <$> readMaybe @Int a <*> pure (map read $ words b)

tryOperators :: [Int -> Int -> Int] -> (Int, [Int]) -> Bool
tryOperators _ (target, []) = target == 0
tryOperators ops (target, x : xs) = go xs x
  where
    go [] !current = target == current
    go (x : xs) !current = target >= current && any (\op -> go xs (current `op` x)) ops

con a b = b + (a * (10 ^ (floor (logBase 10 (fromIntegral b)) + 1)))

fromEnd :: [Int -> Int -> Maybe Int] -> (Int, [Int]) -> Bool
fromEnd ops (target, xs) = any (go target (reverse xs)) ops
  where
    go target [] _ = target == 0
    go target (x : xs) op = maybe False (\t -> any (go t xs) ops) (op x target)

addOp x target
  | target' < 0 = Nothing
  | otherwise = Just target'
  where
    target' = target - x

mulOp x target
  | x <= 0 || r /= 0 = Nothing
  | otherwise = Just d
  where
    (d, r) = target `divMod` x

conOp x target
  | m == x = Just d
  | otherwise = Nothing
  where
    n = 10 ^ (floor (logBase 10 (fromIntegral x)) + 1)
    (d, m) = target `divMod` n

day7 :: IO (String, String)
day7 = do
  -- input <- mapMaybe inputParser . lines <$> (readFile . (++ "/input/test7.txt") =<< getDataDir)
  input <- mapMaybe inputParser . lines <$> (readFile . (++ "/input/input7.txt") =<< getDataDir)
  let !finalAnsa =
        show
          . sum
          . map fst
          $ filter ((||) <$> fromEnd [addOp, mulOp] <*> tryOperators [(+), (*)]) input
  let !finalAnsb =
        show
          . sum
          . map fst
          $ filter ((||) <$> fromEnd [addOp, mulOp, conOp] <*> tryOperators [(+), (*), con]) input
  pure (finalAnsa, finalAnsb)
