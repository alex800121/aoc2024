module Day1 where

import Data.List (sort, transpose, group)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)

day1 :: IO ()
day1 = do
  [a, b] <- map sort . transpose . map (map (read @Int) . words) . lines <$> readFile "input/input1.txt"
  putStrLn
    . ("day1: " ++)
    . show
    . sum
    . map abs
    $ zipWith subtract a b
  let f = IM.fromList . map ((,) <$> head <*> length) . group
      a' = f a
      b' = f b
  putStrLn
    . ("day1: " ++)
    . show
    $ IM.foldrWithKey (\k v acc -> fromMaybe 0 (b' IM.!? k) * v * k + acc) 0 a'

