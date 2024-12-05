module Day4 where

import Data.Array.Unboxed (UArray, indices, (!?))
import Data.Bifunctor (Bifunctor (bimap, first))
import MyLib (drawArray)

type Index = (Int, Int)

testXMAS2 :: Index -> UArray Index Char -> [(Index, Char)] -> Bool
testXMAS2 xy@(x, y) arr = all (\(i, c) -> Just c == (arr !? bimap (+ x) (+ y) i))

directions = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], not (x == 0 && y == 0)]

rotate :: Index -> Index
rotate (x, y) = (y, -x)

xmas1 = map (\(x, y) -> (`zip` "XMAS") . take 4 $ iterate (bimap (+ x) (+ y)) (0, 0)) directions

xmas2 =
  take 4 $
    iterate
      (map (first rotate))
      [ ((0, 0), 'A'),
        ((-1, -1), 'M'),
        ((-1, 1), 'M'),
        ((1, -1), 'S'),
        ((1, 1), 'S')
      ]

solve input x =
  length
    [ ()
      | i <- indices input,
        xmas <- x,
        testXMAS2 i input xmas
    ]

day4 :: IO ()
day4 = do
  -- input <- drawArray @UArray . lines <$> readFile "input/test4.txt"
  input <- drawArray @UArray . lines <$> readFile "input/input4.txt"
  putStrLn
    . ("day4a: " ++)
    . show
    $ solve input xmas1
  putStrLn
    . ("day4b: " ++)
    . show
    $ solve input xmas2
