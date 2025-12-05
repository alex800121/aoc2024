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
solveX ((a, b, c), (d, e, f))
  | m /= 0 && mx == 0 && my == 0 = Just $ 3 * dx + dy
  | otherwise = Nothing
  where
    m = a * e - b * d
    cx = c * e - b * f
    cy = a * f - c * d
    (dx, mx) = cx `divMod` m
    (dy, my) = cy `divMod` m

modifyInput :: Input -> Input
modifyInput ((a, b, x), (c, d, y)) = ((a, b, x + 10000000000000), (c, d, y + 10000000000000))

day13 :: IO (String, String)
day13 = do
  -- print $ emcd 2 3
  input <- map (fromJust . parseMaybe inputParser) . splitOn "\n\n" <$> (readFile . (++ "/input/input13.txt") =<< getDataDir)
  -- input <- map (fromJust . parseMaybe inputParser) . splitOn "\n\n" <$> (readFile . (++ "/input/test13.txt") =<< getDataDir)
  let !finalAnsa =
        show
          . sum
          $ mapMaybe solveX input
  let !finalAnsb =
        show
          . sum
          $ mapMaybe (solveX . modifyInput) input
  pure (finalAnsa, finalAnsb)
