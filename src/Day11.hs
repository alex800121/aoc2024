module Day11 where

import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace (traceShow)
import Paths_AOC2024 (getDataDir)

log10 b | b <= 0 = 1
log10 b = floor (logBase 10 (fromIntegral b)) + 1

step i
  -- \| traceShow (i, l) False = undefined
  | i == 0 = [1]
  | even l = [i `div` l2, i `mod` l2]
  | otherwise = [i * 2024]
  where
    l = log10 i
    l2 = 10 ^ (l `div` 2)

blink :: Map Integer Int -> Map Integer Int
blink = Map.foldlWithKey' (\acc k e -> Map.unionsWith (+) (acc : map (`Map.singleton` e) (step k))) Map.empty

day11 :: IO (String, String)
day11 = do
  input <- Map.unionsWith (+) . map (uncurry Map.singleton . (,1) . read @Integer) . words <$> (readFile . (++ "/input/input11.txt") =<< getDataDir)
  let !finalAnsa =
        show
          . sum
          $ iterate blink input !! 25
  let !finalAnsb =
        show
          . sum
          $ iterate blink input !! 75
  pure (finalAnsa, finalAnsb)
