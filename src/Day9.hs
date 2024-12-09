module Day9 where

import Control.Arrow ((>>>))
import Control.Monad (when, (>=>))
import Control.Monad.ST.Strict (ST, fixST, runST)
import Data.Char (digitToInt, isDigit)
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Vector (Vector, freeze, ifoldl', takeWhile, thaw)
import Data.Vector.Mutable (MVector, STVector, generate, new, read, write)
import Debug.Trace (traceShow)
import Paths_AOC2024 (getDataDir)

data Disk = Space | ID Int deriving (Show, Eq)

readInputB = go 0 0 (IM.empty, IM.empty)
  where
    go i pos (s, t) (x : xs) | isDigit x = go' i (pos + digitToInt x) (IM.insert i (pos, digitToInt x) s, t) xs
    go _ _ s _ = s
    go' i pos (s, t) (x : xs) | isDigit x = go (i + 1) (pos + digitToInt x) (s, IM.insertWith (<>) (digitToInt x) (IS.singleton pos) t) xs
    go' _ _ s _ = s

condenseB :: (IntMap (Int, Int), IntMap IntSet) -> (IntMap (Int, Int), IntMap IntSet)
condenseB (s, t) = go li (s, t)
  where
    li = sortBy (comparing Down) (IM.keys s)
    go (x : xs) (s, t)
      -- \| traceShow (s, t) False = undefined
      | Just (oldPos, len) <- s IM.!? x,
        ((newPos, rest), len') : _ <- sortBy (compare `on` fst . fst) $ mapMaybe (liftA2 (,) <$> ((t IM.!?) >=> IS.minView) <*> pure) [len .. 9],
        -- newLen <- len' - len = traceShow (x, newLen, newPos, len) $
        newLen <- len' - len,
        newPos < oldPos =
          go xs (IM.insert x (newPos, len) s, IM.insertWith (<>) newLen (IS.singleton (newPos + len)) $ IM.insert len' rest t)
      | otherwise = go xs (s, t)
    go _ o = o

readInputA s = runST $ do
  output <- generate l (const Space) :: ST s (STVector s Disk)
  f 0 0 output xs
  freeze output
  where
    xs = map digitToInt $ init s
    l = sum xs
    f _ _ _ [] = pure ()
    f i n o (y : ys) = mapM_ (\j -> write o j (ID i)) [n .. (n + y - 1)] >> g i (n + y) o ys
    g _ _ _ [] = pure ()
    g i n o (y : ys) = f (i + 1) (n + y) o ys

condenseA v = runST $ do
  mv <- thaw v :: ST s (STVector s Disk)
  go 0 (length v - 1) mv
  freeze mv
  where
    go :: Int -> Int -> STVector s Disk -> ST s ()
    go i j k = do
      i' <- f i
      (j', x) <- g j
      when (i' < j') $ do
        write k i' x
        write k j' Space
        go i' j' k
      where
        f n = Data.Vector.Mutable.read k n >>= \x -> if x == Space then pure n else f (n + 1)
        g n = Data.Vector.Mutable.read k n >>= \x -> if x /= Space then pure (n, x) else g (n - 1)

checkSumA = ifoldl' f 0
  where
    f acc _ Space = acc
    f acc i (ID x) = acc + (i * x)

checkSumB :: IntMap (Int, Int) -> Int
checkSumB = IM.foldlWithKey (\acc k (pos, len) -> acc + ((((pos + (pos + len - 1)) * len) `div` 2) * k)) 0

day9 :: IO ()
day9 = do
  input <- readFile . (++ "/input/input9.txt") =<< getDataDir
  -- input <- readFile . (++ "/input/test9.txt") =<< getDataDir
  let inputA = readInputA input
      inputB = readInputB input
  putStrLn
    . ("Day9a: " ++)
    . show
    . checkSumA
    $ condenseA inputA
  putStrLn
    . ("Day9b: " ++)
    . show
    . checkSumB
    . fst
    $ condenseB inputB
