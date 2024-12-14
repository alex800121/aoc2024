
module Day14 where

import Data.Bifunctor (Bifunctor (..))
import Data.Function (on)
import Data.List (find, partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import MyLib (Nat (..), Parser, Vec (..), drawGraph, signedInteger, vZipWith)
import Paths_AOC2024 (getDataDir)
import Text.Megaparsec (parseMaybe, parseTest)
import Text.Megaparsec.Char (char, string)

width = 101

height = 103

-- width = 11
--
-- height = 7

wm = width `div` 2

hm = height `div` 2

type S2 = S (S Z)

type Star = Vec S2 (Int, Int)

type Index = Vec S2 Int

-- p=41,52 v=-2,37
inputParser :: Parser Star
inputParser = do
  x <- string "p=" >> signedInteger
  y <- char ',' >> signedInteger
  vx <- string " v=" >> signedInteger
  vy <- char ',' >> signedInteger
  pure $ Cons (x, vx) (Cons (y, vy) Nil)

starPos :: Index -> Int -> Star -> Index
starPos i n s = vZipWith (\(x, vx) w -> (x + vx * n) `mod` w) s i

quadrant :: [Index] -> ((Int, Int), (Int, Int))
quadrant = bimap fy fy . fx
  where
    fx [] = ([], [])
    fx (c@(Cons x _) : xs)
      | x < wm = first (c :) $ fx xs
      | x > wm = second (c :) $ fx xs
      | otherwise = fx xs
    fy [] = (0, 0)
    fy (c@(Cons _ (Cons y _)) : ys)
      | y < hm = first (1 +) $ fy ys
      | y > hm = second (1 +) $ fy ys
      | otherwise = fy ys

deleteSymmetry :: [Index] -> Int
deleteSymmetry = Map.size . Map.filter (== 1) . foldr (\x -> Map.insertWith (+) x 1) Map.empty . map (\(Cons x y) -> Cons (wm - abs (wm - x)) y)

day14 :: IO ()
day14 = do
  -- input <- mapMaybe (parseMaybe inputParser) . lines <$> (readFile . (++ "/input/test14.txt") =<< getDataDir)
  input <- mapMaybe (parseMaybe inputParser) . lines <$> (readFile . (++ "/input/input14.txt") =<< getDataDir)
  -- print $ quadrant $ map (starPos (Cons width (Cons height Nil)) 100) input
  print $ uncurry (*) $ bimap (uncurry (*)) (uncurry (*)) $ quadrant $ map (starPos (Cons width (Cons height Nil)) 100) input
  -- mapM_ putStrLn $ map (unlines . drawGraph (\case Just _ -> '#'; _ -> ' ') . Map.fromList . map (\(Cons x (Cons y _)) -> ((x, y), ()))) $  map (\n -> map (starPos (Cons width (Cons height Nil)) n) input) [0..]
  mapM_ (appendFile "output14") $ map (unlines . drawGraph (\case Just _ -> '#'; _ -> ' ') . Map.fromList . map (\(Cons x (Cons y _)) -> ((x, y), ()))) $ filter ((< 100) . deleteSymmetry) $ map (\n -> map (starPos (Cons width (Cons height Nil)) n) input) [0 ..]
