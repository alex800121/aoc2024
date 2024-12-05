module Day1 where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List (group, sort, transpose)
import Data.Maybe (fromMaybe)
import Paths_AOC2024 (getDataDir)

day1 :: IO ()
day1 = do
  [a, b] <-
    map sort
      . transpose
      . map (map (read @Int) . words)
      . lines
      <$> (readFile . (++ "/input/input1.txt") =<< getDataDir)
  putStrLn
    . ("day1a: " ++)
    . show
    . sum
    . map abs
    $ zipWith subtract a b
  let f = IM.fromList . map ((,) <$> head <*> length) . group
      a' = f a
      b' = f b
  putStrLn
    . ("day1b: " ++)
    . show
    $ IM.foldrWithKey (\k v acc -> fromMaybe 0 (b' IM.!? k) * v * k + acc) 0 a'
