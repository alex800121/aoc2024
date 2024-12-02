module Day2 where

import MyLib (pickAnySplit)

day2 :: IO ()
day2 = do
  input <- map (map (read @Int) . words) . lines <$> readFile "input/input2.txt"
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
solveA l = all (((&&) <$> (>= 1) <*> (<= 3)) . abs) l' && ((||) <$> all (>= 0) <*> all (< 0)) l'
  where
    l' = zipWith subtract l (tail l)

solveB :: [Int] -> Bool
solveB l = any solveA l'
  where
    l' = l : map snd (pickAnySplit l)
