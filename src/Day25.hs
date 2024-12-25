module Day25 where

import Data.Either (partitionEithers)
import Data.List (group, transpose)
import Data.List.Split (splitOn)
import Paths_AOC2024 (getDataDir)

readInput s = partitionEithers l
  where
    f = map (length . head . group) . transpose . lines
    g f x = if head x == '.' then Left (f x) else Right (f x)
    l = map (g f) $ splitOn "\n\n" s

day25 :: IO ()
day25 = do
  (keys, locks) <- readInput <$> (readFile . (++ "/input/input25.txt") =<< getDataDir)
  -- (keys, locks) <- readInput <$> (readFile . (++ "/input/test25.txt") =<< getDataDir)
  putStrLn
    . ("day25a: " ++)
    . show
    $ length [(k, l) | k <- keys, l <- locks, and (zipWith (>=) k l)]
