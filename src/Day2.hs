module Day2 where

import MyLib (pickAnySplit)
import Paths_AOC2024 (getDataDir)

day2 :: IO ()
day2 = do
  input <- map (map (read @Int) . words) . lines <$> (readFile . (++ "/input/input2.txt") =<< getDataDir)
  putStrLn
    . ("day2a: " ++)
    . show
    . length
    $ filter solveA input
  putStrLn
    . ("day2b: " ++)
    . show
    . length
    $ filter solveB input

solveA :: [Int] -> Bool
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
