module Day3 where

import Control.Arrow
import Data.Char (isDigit)
import Data.List
import Data.Maybe (mapMaybe)
import Debug.Trace
import Paths_AOC2024 (getDataDir)
import Text.Read (readMaybe)

data Mul a = Mul a a
  deriving (Eq, Show)

data Switch = Do | Dont
  deriving (Show, Eq)

data SMul a = S Switch | M (Mul a)
  deriving (Eq, Show)

parse :: String -> [SMul Int]
parse [] = []
parse input
  | Just s0 <- stripPrefix "mul(" input,
    (x0, s1) <- span isDigit s0,
    Just x <- readMaybe @Int x0,
    ',' : s2 <- s1,
    (y0, s3) <- span isDigit s2,
    Just y <- readMaybe @Int y0,
    ')' : s <- s3 =
      M (Mul x y) : parse s
  | Just s <- stripPrefix "do()" input = S Do : parse s
  | Just s <- stripPrefix "don't()" input = S Dont : parse s
parse (_ : input) = parse input

day3 :: IO (String, String)
day3 = do
  input <- readFile . (++ "/input/input3.txt") =<< getDataDir
  let !finalAnsa =
        show
          . sum
          . map
            ( \case
                M (Mul x y) -> x * y
                _ -> 0
            )
          $ parse input
  let !finalAnsb =
        show
          . fst
          . foldl'
            ( \(acc, b) i -> case i of
                S Do -> (acc, True)
                S Dont -> (acc, False)
                M (Mul x y) | b -> (acc + (x * y), b)
                _ -> (acc, b)
            )
            (0, True)
          $ parse input
  pure (finalAnsa, finalAnsb)
