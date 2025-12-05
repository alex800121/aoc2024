module Day19 where

import Data.Array.IArray qualified as A
import Data.Array.Unboxed (Array)
import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as Set
import Paths_AOC2024 (getDataDir)

readInput s = (rules, targets)
 where
  [a, b] = splitOn "\n\n" s
  rules = Set.fromList $ splitOn ", " a
  targets = lines b

towelComb rules target = v A.! l
 where
  maxN = maximum $ Set.map length rules
  l = length target
  v = A.array (0, l) [(i, f i) | i <- [0 .. l]] :: Array Int Int
  f 0 = 1
  f n =
    sum
      [ v A.! top
      | bot <- [1 .. min n maxN]
      , let top = n - bot
      , let s = take bot $ drop top target
      , s `Set.member` rules
      ]

day19 :: IO (String, String)
day19 = do
  (rules, targets) <- readInput <$> (readFile . (++ "/input/input19.txt") =<< getDataDir)
  -- (rules, targets) <- readInput <$> (readFile . (++ "/input/test19.txt") =<< getDataDir)
  let ans = map (towelComb rules) targets
  let
    !finalAnsa =
      show
        . length
        $ filter (> 0) ans
  let
    !finalAnsb =
      show $
        sum ans
  pure (finalAnsa, finalAnsb)
