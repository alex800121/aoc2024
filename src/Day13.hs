module Day13 where

import Data.Bifunctor (Bifunctor (..))
import Data.Function (on)
import Data.List (sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Debug.Trace (traceShow)
import MyLib (Parser, emcd, signedInteger)
import Paths_AOC2024 (getDataDir)
import Text.Megaparsec
import Text.Megaparsec.Char

type Input = ((Int, Int, Int), (Int, Int, Int))

inputParser :: Parser Input
inputParser = do
  ax <- string "Button A: X+" >> fromIntegral <$> signedInteger
  ay <- string ", Y+" >> fromIntegral <$> signedInteger <* newline
  bx <- string "Button B: X+" >> fromIntegral <$> signedInteger
  by <- string ", Y+" >> fromIntegral <$> signedInteger <* newline
  targetx <- string "Prize: X=" >> fromIntegral <$> signedInteger
  targety <- string ", Y=" >> fromIntegral <$> signedInteger <* optional newline
  pure ((ax, bx, targetx), (ay, by, targety))

-- Button A: X+18, Y+53
-- Button B: X+61, Y+50
-- Prize: X=5128, Y=6804

-- solveX :: Input -> [(Int, Int)]
solveX ((ax, bx, targetx), (ay, by, targety)) =
  sort
    [ 3 * a + b
      | let (cx, xa, xb) = emcd ax bx,
        let (cy, ya, yb) = emcd ay by,
        -- traceShow (c, la, lb) True,
        targetx `mod` cx == 0,
        targety `mod` cy == 0,
        let timex = targetx `div` cx,
        let timey = targety `div` cy,
        let (xa', xb') = (xa * timex, xb * timex),
        let (ya', yb') = (ya * timey, yb * timey),
        (a, b) <- f xa' xb' ax bx,
        (a, b) `elem` f ya' yb' ay by
    ]
  where
    f a b ax bx
      | a <= 0 = takeWhile (\(x, y) -> x >= 0 && y >= 0) $ iterate (bimap (+ bx') (subtract ax')) (a + (bx' * da), b - (ax' * da))
      | b <= 0 = takeWhile (\(x, y) -> x >= 0 && y >= 0) $ iterate (bimap (subtract bx') (+ ax')) (a - (bx' * db), b + (ax' * db))
      where
        c = gcd ax bx
        ax' = ax `div` c
        bx' = bx `div` c
        da = negate (a `div` bx')
        db = negate (b `div` ax')

modifyInput :: Input -> Input
modifyInput ((a, b, x), (c, d, y)) = ((a, b, x + 10000000000000), (c, d, y + 10000000000000))

day13 :: IO ()
day13 = do
  -- print $ emcd 2 3
  input <- map (fromJust . parseMaybe inputParser) . splitOn "\n\n" <$> (readFile . (++ "/input/input13.txt") =<< getDataDir)
  input <- map (fromJust . parseMaybe inputParser) . splitOn "\n\n" <$> (readFile . (++ "/input/test13.txt") =<< getDataDir)
  print $ sum $ concatMap (take 1 . solveX) input
  print $ sum $ concatMap (take 1 . solveX . modifyInput) input
