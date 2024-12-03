module Day3 where

day3 :: IO ()
day3 = do
  input <- map (map (read @Int) . words) . lines <$> readFile "input/input3.txt"
  pure ()
