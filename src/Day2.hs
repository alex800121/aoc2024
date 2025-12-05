module Day2 where

import MyLib (pickAnySplit)
import Paths_AOC2024 (getDataDir)

solveA ::
  [Int] -> Bool
solveA =
  ( (&&)
      <$> all (((&&) <$> (>= 1) <*> (<= 3)) . abs)
      <*> ( (||)
              <$> all (>= 0)
              <*> all (< 0)
          )
  )
    . (zipWith subtract <$> id <*> tail)

solveB :: [Int] -> Bool
solveB = any solveA . ((:) <$> id <*> map snd . pickAnySplit)

day2 :: IO (String, String)
day2 = do
  input <- map (map (read @Int) . words) . lines <$> (readFile . (++ "/input/input2.txt") =<< getDataDir)
  let !finalAnsa =
        show
          . length
          $ filter solveA input
  let !finalAnsb =
        show
          . length
          $ filter solveB input
  pure (finalAnsa, finalAnsb)
