module Day24 where

import Control.Monad (guard, when)
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.Char (toLower, toUpper)
import Data.Either.Combinators
import Data.List (foldl', intercalate, sort)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Debug.Trace (traceM, traceShow)
import Paths_AOC2024 (getDataDir)
import Text.Read (readMaybe)

data C = X | Y | Z deriving (Show, Ord, Eq, Read)

data Reg = Reg C Int | Wire String deriving (Show, Ord, Eq)

data Gate = AND | OR | XOR deriving (Show, Ord, Read, Eq)

type Ins = Map Reg (Gate, Reg, Reg)

buildAdder m cx i
  | Just c <- cx = first (i,) do
      xxory <- f [] (XOR, xi, yi)
      xandy <- f [(XOR, xi, yi)] (AND, xi, yi)
      cxorab <- f [(AND, xi, yi), (XOR, xi, yi)] (XOR, xxory, c)
      candab <- f [(XOR, xxory, c), (AND, xi, yi), (XOR, xi, yi)] (AND, xxory, c)
      if cxorab /= Reg Z i
        then Left [(x, m Map.!? x) | x <- [(OR, xandy, candab), (AND, xxory, c), (XOR, xxory, c), (AND, xi, yi), (XOR, xi, yi)]]
        else do
          f [(XOR, xxory, c), (AND, xi, yi), (XOR, xi, yi), (AND, xxory, c)] (OR, xandy, candab)
  | Nothing <- cx = first (i,) do
      xxory <- f [] (XOR, xi, yi)
      if xxory /= Reg Z i then Left [(x, m Map.!? x) | x <- [(XOR, xi, yi)]] else f [(XOR, xi, yi)] (AND, xi, yi)
  where
    xi = Reg X i
    yi = Reg Y i
    f acc a = maybeToRight ([(x, m Map.!? x) | x <- a : acc]) (m Map.!? a)

buildAdderList n m ins = go m ins Nothing 0
  where
    go _ _ _ i | i == n = pure []
    go m ins c i = case buildAdder m c i of
      Left (i0, xs) -> do
        (_, Just x) <- xs
        (_, Just y) <- xs
        guard (x > y)
        let ins' = Map.insert x (ins Map.! y) (Map.insert y (ins Map.! x) ins)
            m' = rev ins'
        case buildAdder m' c i of
          Left (i1, _) | i1 <= i0 -> []
          _ -> ([x, y] <>) <$> go m' ins' c i
      Right c' -> go m ins (Just c') (i + 1)

readReg s
  | x : xs <- s, Just i <- readMaybe @Int xs = Reg (read [toUpper x]) i
  | otherwise = Wire s

readIns s
  | a : gate : b : _ : c : _ <- words s, g <- read @Gate gate = (readReg c, (g, readReg a, readReg b))

readInput :: String -> (Int, Int, Ins)
readInput s = (x, y, ins)
  where
    [a, b] = splitOn "\n\n" s
    (x, y) = foldl' f (0, 0) $ lines a
    f (x, y) s
      | c : xs <- s,
        [a, 1] <- map (read @Int) $ splitOn ": " xs =
          if c == 'x' then (x `setBit` a, y) else (x, y `setBit` a)
      | otherwise = (x, y)
    ins = Map.fromList $ map readIns $ lines b

readOp AND = (&&)
readOp OR = (||)
readOp XOR = (/=)

processAll ins x y =
  foldl'
    (\m k -> maybe m fst $ process ins x y k m Set.empty Set.empty)
    Map.empty
    (Map.keys ins)

process :: Map Reg (Gate, Reg, Reg) -> Int -> Int -> Reg -> Map Reg Bool -> Set Reg -> Set (Reg, Set Reg) -> Maybe (Map Reg Bool, (Bool, Set Reg, Set (Reg, Set Reg)))
process ins x y reg m tra loo
  | (reg, tra) `Set.member` loo = Nothing
  | Just a <- m Map.!? reg = Just (m, (a, tra', loo'))
  | Reg X i <- reg, a <- x `testBit` i = Just (Map.insert reg a m, (a, tra', loo'))
  | Reg Y i <- reg, a <- y `testBit` i = Just (Map.insert reg a m, (a, tra', loo'))
  | Just (op, a, b) <- ins Map.!? reg,
    Just (m0, (procA, tra0, loo0)) <- process ins x y a m tra' loo',
    Just (m1, (procB, tra1, loo1)) <- process ins x y b m0 tra0 loo0,
    x <- readOp op procA procB =
      Just (Map.insert reg x m1, (x, tra1, loo1))
  | otherwise = Nothing
  where
    tra' = Set.insert reg tra
    loo' = Set.insert (reg, tra) loo

showReg (Reg x i) = map toLower (show x) <> reverse (take 2 (reverse (show i) <> repeat '0'))
showReg (Wire x) = x

rev =
  Map.fromList
    . concatMap (\(k, (a, b, c)) -> [((a, b, c), k), ((a, c, b), k)])
    . Map.toList

day24 :: IO ()
day24 = do
  (x, y, ins) <- readInput <$> (readFile . (++ "/input/input24.txt") =<< getDataDir)
  putStrLn
    . ("day24a: " ++)
    . show
    . Map.foldlWithKey' (\acc k b -> case k of Reg Z i | b -> acc `setBit` i; _ -> acc) (0 :: Int)
    $ processAll ins x y
  let revins = rev ins
      ansB = head $ buildAdderList 45 revins ins
  putStrLn
    . ("day24b: " ++)
    . intercalate ","
    $ sort [showReg x | x <- ansB]
