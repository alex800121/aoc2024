{-# LANGUAGE MultiWayIf #-}
module Day22 where

import Control.Monad.ST (ST, runST)
import Data.Bits (Bits (..))
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as M
import Paths_AOC2024 (getDataDir)
import Control.Monad (when)

mixPrune f = (.&. (16777216 - 1)) . (\x -> xor x (f x)) :: Int -> Int

step = mixPrune (`shiftL` 11) . mixPrune (`shiftR` 5) . mixPrune (`shiftL` 6)

toIM :: [Int] -> Int
toIM = foldr (\x acc -> acc * 20 + (x + 9)) 0

solveBST' time input@(x : xs) =  runST (M.replicate (20 ^ 4) (0 :: Int) >>= go (time - 4) 0 1 ds x5 xs)
  where
    fds = ((,) <$> (zipWith subtract <$> map (`mod` 10) <*> map (`mod` 10) . tail) <*> last) . take 5 . iterate step
    (ds, x5) = fds x
    n = length input
    go :: Int -> Int -> Int -> [Int] -> Int -> [Int] -> M.STVector s Int -> ST s (Int, Int)
    go !t !s !round ds@[!d0, !d1, !d2, !d3] !x xs v = do
      -- traceM $ show (ds, x)
      (m0, round0) <- (`divMod` n) <$> M.read v i
      let m1 = m0 + (x `mod` 10)
      when (round0 /= round) $ M.write v i (round + m1 * n)
      if | t <= 0, [] <- xs -> (s', ) <$> M.foldl' (\acc y -> max acc (y `div` n)) 0 v
         | t <= 0, y : xs1 <- xs, (ds1, x1) <- fds y -> go (time - 4) s' (succ round) ds1 x1 xs1 v
         | otherwise -> go (pred t) s round ds' x' xs v
      where
        i = toIM ds
        s' = s + x
        x' = step x
        ds' = [d1, d2, d3, (x' `mod` 10) - (x `mod` 10)]

solveBST input = runST (M.replicate (20 ^ 4) (0 :: Int) >>= go 1 (map (map (`mod` 10)) input))
  where
    n = length input
    go :: Int -> [[Int]] -> M.STVector s Int -> ST s Int
    go round [] v = M.foldl' (\acc x -> max acc (x `div` n)) 0 v
    go round ((a : b : c : d : e : xs) : ys) v = do
      let i = toIM [b - a, c - b, d - c, e - d]
      (m0, round0) <- (`divMod` n) <$> M.read v i
      let m1 = m0 + e
      if round0 /= round
        then M.write v i (round + m1 * n) >> go round ((b : c : d : e : xs) : ys) v
        else go round ((b : c : d : e : xs) : ys) v
    go round (_ : ys) v = go (round + 1) ys v

day22 :: IO ()
day22 = do
  input <- map (read @Int) . lines <$> (readFile . (++ "/input/input22.txt") =<< getDataDir)
  -- input <- map (read @Int) . lines <$> (readFile . (++ "/input/test22.txt") =<< getDataDir)
  let (ansA, ansB) = solveBST' 2000 input
  putStrLn
    . ("day22a: " ++)
    . show
    $ ansA
  putStrLn
    . ("day22b: " ++)
    . show
    $ ansB
