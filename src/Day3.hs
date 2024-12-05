{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Day3 where

import Control.Arrow
import Control.Monad.Hefty
import Control.Monad.Hefty.NonDet
import Control.Monad.Hefty.State
import qualified Data.Effect.NonDet as NonDet
import Data.Maybe (mapMaybe)
import HeftiaParser
import Paths_AOC2024 (getDataDir)

data Mul a = Mul a a
  deriving (Eq, Show)

data Switch = Do | Dont
  deriving (Show, Eq)

data SMul a = S Switch | M (Mul a)
  deriving (Eq, Show)

mulParser :: forall eh ef. (Choose <| ef, Empty <| ef, State String <| ef) => Eff eh ef (Mul Int)
mulParser = do
  string "mul("
  a <- signedInteger
  char ','
  b <- signedInteger
  char ')'
  if a >= 0 && a < 1000 && b >= 0 && b < 1000 then pure (Mul a b) else NonDet.empty

manyMul :: forall eh ef. (Choose <| ef, Empty <| ef, State String <| ef) => Eff eh ef [Mul Int]
manyMul =
  (eof @_ @eh >> pure [])
    `branch`  ((:) <$> mulParser <*> manyMul)
    `branch` (anySingle >> manyMul)

switchParser :: forall eh ef. (Choose <| ef, Empty <| ef, State String <| ef) => Eff eh ef Switch
switchParser =
  (string "do()" >> pure Do) `branch` (string "don't()" >> pure Dont)

parseTillDo :: forall eh ef. (Choose <| ef, Empty <| ef, State String <| ef) => Eff eh ef [Mul Int]
parseTillDo =
  (eof >> pure [])
    `branch` (string "do()" >> manyMulSwitch)
    `branch` (anySingle >> parseTillDo)

manyMulSwitch :: forall eh ef. (Choose <| ef, Empty <| ef, State String <| ef) => Eff eh ef [Mul Int]
manyMulSwitch =
  (eof >> pure [])
    `branch` (string "don't()" >> parseTillDo)
    `branch` ((:) <$> mulParser <*> manyMulSwitch)
    `branch` (anySingle >> manyMulSwitch)

manySMul :: forall eh ef. (Choose <| ef, Empty <| ef, State String <| ef) => Eff eh ef [SMul Int]
manySMul =
  (eof >> pure [])
    `branch` ((:) . S <$> switchParser <*> manySMul)
    `branch` ((:) . M <$> mulParser <*> manySMul)
    `branch` (anySingle >> manySMul)

applyMul :: Mul Int -> Int
applyMul (Mul x y) = x * y

day3 :: IO ()
day3 = do
  input <- readFile . (++ "/input/input3.txt") =<< getDataDir
  putStrLn
    . ("day3a: " ++)
    . show
    . fmap (sum . map applyMul)
    . runPure
    . evalState input
    . runNonDetMaybe
    $ manyMul <* eof
  putStrLn
    . ("day3b: " ++)
    . show
    . fmap (sum . map applyMul)
    . runPure
    . evalState input
    . runNonDetMaybe
    $ manyMulSwitch <* eof
