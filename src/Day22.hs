module Day22 where

import Control.Monad.ST (ST, runST)
import Data.Bits (Bits (..))
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (foldl')
import Data.List.Split (divvy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as M
import Paths_AOC2024 (getDataDir)

mixPrune f = (.&. (16777216 - 1)) . (\x -> xor x (f x)) :: Int -> Int

step = mixPrune (`shiftL` 11) . mixPrune (`shiftR` 5) . mixPrune (`shiftL` 6)

toIM = foldr (\x acc -> acc * 20 + (x + 9)) 0

solveBST input = runST (M.replicate (20 ^ 4) (0 :: Int, 0 :: Int) >>= go 1 (map (map (`mod` 10)) input))
  where
    go :: Int -> [[Int]] -> M.STVector s (Int, Int) -> ST s Int
    go round [] v = M.foldl' (\acc x -> max acc (snd x)) 0 v
    go round ((a : b : c : d : e : xs) : ys) v = do
      let i = toIM [b - a, c - b, d - c, e - d]
      (round0, m0) <- M.read v i
      let m1 = m0 + e
      if round0 /= round
        then M.write v i (round, m1) >> go round ((b : c : d : e : xs) : ys) v
        else go round ((b : c : d : e : xs) : ys) v
    go round (_ : ys) v = go (round + 1) ys v
solveB =
  maximum
    . IM.unionsWith (+)
    . map
      ( IM.fromListWith (const id)
          . ( zip
                <$> ( map toIM
                        . divvy 4 1
                        . (zipWith subtract <*> tail)
                    )
                <*> drop 4
            )
          . map (`mod` 10)
      )

day22 :: IO ()
day22 = do
  input <- map (read @Int) . lines <$> (readFile . (++ "/input/input22.txt") =<< getDataDir)
  let ansA = map (take 2001 . iterate step) input
  putStrLn
    . ("day22a: " ++)
    . show
    . sum
    $ map last ansA
  putStrLn
    . ("day22b: " ++)
    . show
    $ solveBST ansA
