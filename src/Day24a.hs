module Day24 where

import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.Char (toLower, toUpper)
import Data.Foldable (find)
import Data.Function (fix)
import Data.List (foldl', intercalate, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceM, traceShow)
import Paths_AOC2024 (getDataDir)
import Text.Read (readMaybe)

type Ans = Map Reg Bool

type Ins = Map Reg Gate

type Gate = Either (Op, Reg, Reg) Bool

data Op = AND | OR | XOR deriving (Show, Eq, Ord, Read)

data XYZ = X | Y | Z deriving (Show, Eq, Read, Ord)

data Reg = Reg XYZ Int | Wire String deriving (Show, Eq, Ord)

readReg a
  | (x : xs) <- a,
    Just x <- readMaybe @XYZ [toUpper x],
    Just xs <- readMaybe @Int xs =
      Just (Reg x xs)
  | otherwise = Just (Wire a)

readInput s
  | [a, b] <- ss,
    xs <- init a,
    Just r <- readReg xs =
      Just (r, Right (b == "1"))
  | [a, b, c, _, d] <- ss,
    Just [a, c, d] <- traverse readReg [a, c, d],
    b <- read @Op b =
      Just (d, Left (b, a, c))
  | otherwise = Nothing
  where
    ss = words s

readOp AND = (&&)
readOp OR = (||)
readOp XOR = (/=)

ansRec ins = m
  where
    m = either (\(x, y, z) -> readOp x (m Map.! y) (m Map.! z)) id <$> ins

ans :: Ins -> Ans -> Reg -> Maybe (Bool, Ans, Set Reg, Set (Reg, Set Reg))
ans ins = go Set.empty Set.empty
  where
    go tra loo acc r
      | (r, tra) `Set.member` loo = Nothing
      | Just b <- acc Map.!? r = Just (b, acc, tra', loo')
      | Just (Right b) <- ins Map.!? r = Just (b, Map.insert r b acc, tra', loo')
      | Just (Left (op, a, b)) <- ins Map.!? r,
        Just (b0, acc0, tra0, loo0) <- go tra' loo' acc a,
        Just (b1, acc1, tra1, loo1) <- go tra0 loo0 acc0 b,
        c <- readOp op b0 b1 =
          Just (c, Map.insert r c acc1, tra1, loo1)
      | otherwise = Nothing
      where
        tra' = Set.insert r tra
        loo' = Set.insert (r, tra') loo

testReg :: Ins -> Int -> (Bool, Set Reg)
testReg ins n = foldl' (\(accB, accS) (b, s) -> (accB && b, accS <> s)) (True, Set.empty) do
  (b, xy) <- test
  let testIns = Map.union (Map.fromList xy) ins
      tested = ans testIns Map.empty z
  case tested of
    Just (b', _, t, _) | b' == b, all noLess t -> pure (True, t)
    _ -> pure (False, Set.empty)
  where
    noLess (Reg _ i) = i <= n
    noLess _ = True
    z = Reg Z n
    x = Reg X n
    y = Reg Y n
    px = Reg X (n - 1)
    py = Reg Y (n - 1)
    testn =
      [ (b, zipWith (\c d -> (c, Right d)) [px, py, x, y] [ipx, ipy, ix, iy])
        | (ipx, ipy) <- [(False, False), (True, True)],
          ix <- [False, True],
          iy <- [False, True],
          let b = (ix /= iy) /= ipx
      ]
    test0 =
      [ (b, zipWith (\c d -> (c, Right d)) [x, y] [ix, iy])
        | ix <- [False, True],
          iy <- [False, True],
          let b = ix /= iy
      ]
    test45 =
      [ (ipx, zipWith (\c d -> (c, Right d)) [px, py] [ipx, ipy])
        | (ipx, ipy) <- [(False, False), (True, True)]
      ]
    test
      | n <= 0 = test0
      | n >= 45 = test45
      | otherwise = testn

testTill :: Ins -> Int -> [Set Reg]
testTill ins limit = go Set.empty Set.empty ins 0
  where
    ks = Map.keysSet ins
    go acc travelled curIns i
      | i > limit = [acc]
      | Set.size acc > 8 = []
      | b = go acc (travelled <> s) curIns (i + 1)
      | otherwise = do
          x <- Set.toList (s Set.\\ travelled)
          y <- Set.toList (ks Set.\\ travelled)
          guard (x /= y)
          guard $ all (\case Reg X _ -> False; Reg Y _ -> False; _ -> True) [x, y]
          guard (x `Set.notMember` acc)
          guard (y `Set.notMember` acc)
          let ins' = Map.insert x (curIns Map.! y) $ Map.insert y (curIns Map.! x) curIns
              (b', _) = testReg ins' i
          guard b'
          -- traceM (show acc)
          go (Set.insert x (Set.insert y acc)) (travelled <> s) ins' (i + 1)
      where
        (b, s) = testReg curIns i

showReg (Reg x i) = map toLower (show x) <> reverse (take 2 (reverse (show i) ++ repeat '0'))
showReg (Wire s) = s

day24 :: IO ()
day24 = do
  ins <- Map.fromList . mapMaybe readInput . lines <$> (readFile . (++ "/input/input24.txt") =<< getDataDir)
  let ansA = ansRec ins
  putStrLn
    . ("day24a: " ++)
    . show
    $ foldl'
      (\acc x -> if ansA Map.! Reg Z x then acc `setBit` x else acc)
      (0 :: Int)
      [0 .. 45]
  putStrLn
    . ("day24b: " ++)
    . intercalate ","
    . sort
    . map showReg
    . Set.toList
    . fromJust
    . find ((== 8) . Set.size)
    $ testTill ins 45
