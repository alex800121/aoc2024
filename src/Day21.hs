module Day21 where

import Data.Bifunctor (Bifunctor (..))
import Data.List (foldl', group, partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Tuple (swap)
import MyLib (Direction (..), drawMap, toIndex)
import Paths_AOC2024 (getDataDir)

type Index = (Int, Int)

type Path = Map (Char, Char) [String]

type Cache = Map (Int, (Char, Char)) Int

(a, b) +^ (c, d) = (a + c, b + d)

drawPad = map swap . Map.toList . drawMap (\case ' ' -> Nothing; c -> Just c)

arrpad =
  drawPad
    [ " ^A"
    , "<v>"
    ]

numpad =
  drawPad
    [ "789"
    , "456"
    , "123"
    , " 0A"
    ]

buildM m =
  Map.fromList
    [ ((c0, c1), s)
    | (c0, i0) <- m
    , (c1, i1) <- m
    , let s = go Set.empty i1 [[(i0, undefined)]]
    ]
 where
  is = map snd m
  go t i1 s
    | not (null es) = map (tail . reverse . ('A' :) . map snd) es
    | null rs = []
    | otherwise = go t' i1 rs'
   where
    (es, rs) = partition ((== i1) . fst . head) s
    t' = t <> Set.fromList (map (fst . head) rs)
    rs' =
      [ (x', "^>v<" !! fromEnum d) : xs
      | xs@((x, _) : _) <- rs
      , x `Set.notMember` t
      , d <- [minBound .. maxBound]
      , let x' = x +^ toIndex d
      , x' `elem` is
      ]

path = buildM arrpad <> buildM numpad

stepChar :: Cache -> Path -> Int -> Char -> Char -> (Int, Cache)
stepChar cache path level start end
  | Just n <- cache Map.!? (level, (start, end)) = (n, cache)
  | Just s <- path Map.!? (start, end)
  , (n, cache') <- foldl' f (maxBound, cache) s =
      (n, Map.insertWith min (level, (start, end)) n cache')
 where
  f (acc, cache) = first (min acc) . stepString cache path (level - 1)

stepString :: Cache -> Path -> Int -> String -> (Int, Cache)
stepString cache path level s
  | level <= 0 = (length s, cache)
  | level > 0, cc <- (zip <*> tail) ('A' : s) = foldl' f (0, cache) cc
 where
  f (acc, cache) = first (+ acc) . uncurry (stepChar cache path level)

day21 :: IO (String, String)
day21 = do
  input <- lines <$> (readFile . (++ "/input/input21.txt") =<< getDataDir)
  -- input <- lines <$> (readFile . (++ "/input/test21.txt") =<< getDataDir)
  let
    !finalAnsa =
      show
        . sum
        $ map ((*) <$> read . init <*> fst . stepString Map.empty path 3) input
  let
    !finalAnsb =
      show
        . sum
        $ map ((*) <$> read . init <*> fst . stepString Map.empty path 26) input
  pure (finalAnsa, finalAnsb)
