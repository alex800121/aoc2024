{-# LANGUAGE MultiWayIf #-}

module Day22 where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Control.Parallel.Strategies
import Data.Bits (Bits (..))
import Data.Function (on)
import Data.List (foldl')
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as M
import Paths_AOC2024 (getDataDir)

mixPrune f = (.&. (16777216 - 1)) . (\x -> xor x (f x)) :: Int -> Int

step = mixPrune (`shiftL` 11) . mixPrune (`shiftR` 5) . mixPrune (`shiftL` 6)

toIM :: [Int] -> Int
toIM = foldl' (\acc x -> acc * 20 + (x + 9)) 0

solveBST' time input@(x : xs) = runST (M.replicate (20 ^ 4) (0, 0) >>= go (time - 4) 0 1 (toIM ds) x5 xs)
 where
  fds = ((,) <$> (zipWith subtract <$> map (`mod` 10) <*> map (`mod` 10) . tail) <*> last) . take 5 . iterate step
  (ds, x5) = fds x
  n = length input
  go :: Int -> Int -> Int -> Int -> Int -> [Int] -> M.STVector s (Int, Int) -> ST s (Int, Int)
  go !t !s !round ds0 !x xs v = do
    (m0, round0) <- M.read v ds0
    let m1 = m0 + (x `mod` 10)
    when (round0 /= round) $ M.write v ds0 (m1, round)
    if
      | t <= 0, [] <- xs -> (s',) <$> M.foldl' (\acc y -> max acc (fst y)) 0 v
      | t <= 0, y : xs1 <- xs, (ds1, x1) <- fds y -> go (time - 4) s' (succ round) (toIM ds1) x1 xs1 v
      | otherwise -> go (pred t) s round ds' x' xs v
   where
    s' = s + x
    x' = step x
    ds' = ((ds0 `mod` (20 ^ 3)) * 20) + (x' `mod` 10) - (x `mod` 10) + 9

day22 :: IO (String, String)
day22 = do
  input <- map (read @Int) . lines <$> (readFile . (++ "/input/input22.txt") =<< getDataDir)
  let (ansA, ansB) = solveBST' 2000 input
  let
    !finalAnsa =
      show $
        ansA
  let
    !finalAnsb =
      show $
        ansB
  pure (finalAnsa, finalAnsb)
