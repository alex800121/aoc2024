module Day7 where

import Control.Monad (unless)
import Control.Monad.Hefty
import Control.Monad.Hefty.NonDet
import Data.List.Split (splitOn)
import Data.Maybe (isJust, mapMaybe, catMaybes, fromMaybe)
import Paths_AOC2024 (getDataDir)
import Text.Read (readMaybe)
import Data.Functor (($>))
import HeftiaParser (runNonDetMaybe)
import Data.List (partition)
import Control.Parallel.Strategies (rpar, parMap)

inputParser :: String -> Maybe (Int, [Int])
inputParser s = do
  let [a, b] = splitOn ": " s
  (,) <$> readMaybe @Int a <*> pure (map read $ words b)

tryOperators :: forall ef eh. (Choose <| ef, Empty <| ef) => [Int -> Int -> Int] -> (Int, [Int]) -> Eff eh ef Int
tryOperators _ (_, []) = empty
tryOperators ops (target, x : xs) = go xs x
  where
    go ys current
      | [] <- ys = if target == current then pure target else empty
      -- | [] <- ys = unless (target == currentInt) empty
      | (x : xs) <- ys, target < current = empty
      | (x : xs) <- ys = do
          op <- choice ops
          go xs (current `op` x)

con a b = b + (a * (10 ^ (floor (logBase 10 (fromIntegral b)) + 1)))

day7 :: IO ()
day7 = do
  -- input <- mapMaybe inputParser . lines <$> (readFile . (++ "/input/test7.txt") =<< getDataDir)
  input <- mapMaybe inputParser . lines <$> (readFile . (++ "/input/input7.txt") =<< getDataDir)
  putStrLn
    . ("day7a: " ++)
    . show
    . sum
    . map fst
    $ filter (isJust . runPure . runNonDetMaybe . tryOperators [(+), (*)])  input
  putStrLn
    . ("day7b: " ++)
    . show
    . sum
    . map fst
    $ filter (isJust . runPure . runNonDetMaybe . tryOperators [(+), (*), con])  input
