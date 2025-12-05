module Day5 where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (partition, sortBy)
import Data.List.Split
import Paths_AOC2024 (getDataDir)

type Rules = IntMap Int

type Book = [Int]

ruleParser = foldr (uncurry (IM.insertWith (<>)) . (\[x, y] -> (x, IS.singleton y)) . map read . splitOn "|") IM.empty

middle l = l !! ((length l - 1) `div` 2)

applyRules rules (x : y : xs) = maybe True (IS.notMember x) (rules IM.!? y) && applyRules rules (y : xs)
applyRules _ _ = True

applyOrdering rules x y = case (rules IM.!? x, rules IM.!? y) of
  (Just a, _) | IS.member y a -> LT
  (_, Just a) | IS.member x a -> GT
  _ -> EQ

day5 :: IO (String, String)
day5 = do
  [a, b] <- map lines . splitOn "\n\n" <$> (readFile . (++ "/input/input5.txt") =<< getDataDir)
  let rules = ruleParser a
      books = map (map (read @Int) . splitOn ",") b
      (correct, incorrect) = partition (applyRules rules) books
  let
    !finalAnsa =
      show
        . sum
        $ map middle correct
  let
    !finalAnsb =
      show
        . sum
        $ map (middle . sortBy (applyOrdering rules)) incorrect
  pure (finalAnsa, finalAnsb)
