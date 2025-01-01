module Day14 where

import Control.Monad (join)
import Data.Bifunctor (Bifunctor (..))
import Data.Function (on)
import Data.List (find, partition, sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import MyLib (Nat (..), Parser, Vec (..), crt, drawGraph, signedInteger, vZipWith)
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

vx :: [Index] -> Double
vx l = sum $ map (\(Cons x (Cons y _)) -> (fromIntegral x - m) ^ 2) l
  where
    m = sum (map (\(Cons x (Cons y _)) -> fromIntegral x) l) / fromIntegral (length l)

vy :: [Index] -> Double
vy l = sum $ map (\(Cons x (Cons y _)) -> (fromIntegral y - m) ^ 2) l
  where
    m = sum (map (\(Cons x (Cons y _)) -> fromIntegral y) l) / fromIntegral (length l)

-- detectLow :: Double -> [Double] -> Maybe Int
detectLow _ [] = Nothing
detectLow factor (x : xs) = go 1 x x xs
  where
    go n s c _ | (s / fromIntegral n) > factor * c = Just $ n - 1
    go _ _ _ [] = Nothing
    go n s c (x : xs) = go (n + 1) (s + c) x xs

draw :: [Index] -> String
draw = unlines . drawGraph (\case Just _ -> '#'; Nothing -> ' ') . Map.fromList . map (\(Cons x (Cons y _)) -> ((x, y), ()))

solveB l = do
  a <- detectLow 2.0 $ map vx l
  b <- detectLow 2.0 $ map vy l
  crt (a, width) (b, height)

day14 :: IO ()
day14 = do
  input <- mapMaybe (parseMaybe inputParser) . lines <$> (readFile . (++ "/input/input14.txt") =<< getDataDir)
  putStrLn
    . ("day14a: " ++)
    . show
    . uncurry (*)
    . bimap (uncurry (*)) (uncurry (*))
    . quadrant
    $ map (starPos (Cons width (Cons height Nil)) 100) input
  putStrLn
    . ("day14b: " ++)
    . show
    . fmap fst
    . solveB
    $ map (\n -> map (starPos (Cons width (Cons height Nil)) n) input) [0 ..]
