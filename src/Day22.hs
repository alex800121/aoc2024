module Day22 where

import Control.Monad.ST (ST, runST)
import Data.Bits (Bits (..))
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (foldl', maximumBy)
import Data.List.Split (divvy)
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as M
import Paths_AOC2024 (getDataDir)
import Data.Array.IArray qualified as I
import Data.Array.ST (STUArray, STArray)
import Data.Array.MArray qualified as MA
import Data.Array.Unboxed (Array)
import qualified Data.Array.IArray as A
import Data.Function (on)

mixPrune f = (.&. (16777216 - 1)) . (\x -> xor x (f x)) :: Int -> Int

step = mixPrune (`shiftL` 11) . mixPrune (`shiftR` 5) . mixPrune (`shiftL` 6)

toIM :: [Int] -> Int
toIM = foldr (\x acc -> acc * 20 + (x + 9)) 0

solveBArray input = maximum . A.amap @Array (`mod` n) . A.accumArray f 0 (0, 20 ^ 4) . go 1 $ map (map (`mod` 10)) input
  where
    n = length input
    f y0 y
      | round0 /= round = round * n + x0 + x
      | otherwise = y0
      where
        (round0, x0) = y0 `divMod` n
        (round, x) = y `divMod` n
    go _ [] = []
    go round ((a : b : c : d : e : xs) : ys) = (toIM [b - a, c - b, d - c, e - d], round * n + e) : go round ((b : c : d : e : xs) : ys)
    go round (_ : ys) = go (round + 1) ys

solveBMap input = maximum $ (`mod` n) <$> go 1 (map (map (`mod` 10)) input) IM.empty
  where
    n = length input
    go round [] v = v
    go round ((a : b : c : d : e : xs) : ys) v
      | Nothing <- v IM.!? i = go round ((b : c : d : e : xs) : ys) (IM.insert i (round * n + e) v)
      | Just (round0, m0) <- (`divMod` n) <$> v IM.!? i,
        m1 <- m0 + e =
          if round0 /= round
            then go round  ((b : c : d : e : xs) : ys) (IM.insert i (round * n + m1) v)
            else go round ((b : c : d : e : xs) : ys) v
      where
        i = toIM [b - a, c - b, d - c, e - d]
    go round (_ : ys) v = go (round + 1) ys v

solveBSTA input = maximum . A.amap (`mod` n) $ runST (MA.newArray (0, 20 ^ 4) (0 :: Int) >>= go 1 (map (map (`mod` 10)) input))
  where
    n = length input
    go :: Int -> [[Int]] -> STUArray s Int Int -> ST s (Array Int Int)
    go round [] v = MA.freeze v
    go round ((a : b : c : d : e : xs) : ys) v = do
      let i = toIM [b - a, c - b, d - c, e - d]
      (round0, m0) <- (`divMod` n) <$> MA.readArray v i
      let m1 = m0 + e
      if round0 /= round
        then MA.writeArray v i (round * n + m1) >> go round  ((b : c : d : e : xs) : ys) v
        else go round ((b : c : d : e : xs) : ys) v
    go round (_ : ys) v = go (round + 1) ys v

solveBST input = runST (M.replicate (20 ^ 4) (0 :: Int) >>= go 1 (map (map (`mod` 10)) input))
  where
    n = length input
    go :: Int -> [[Int]] -> M.STVector s Int -> ST s Int
    go round [] v = M.foldl' (\acc x -> max acc (x `mod` n)) 0 v
    go round ((a : b : c : d : e : xs) : ys) v = do
      let i = toIM [b - a, c - b, d - c, e - d]
      (round0, m0) <- (`divMod` n) <$> M.read v i
      let m1 = m0 + e
      if round0 /= round
        then M.write v i (round * n + m1) >> go round ((b : c : d : e : xs) : ys) v
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
