{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Day3 where

import Control.Applicative (Alternative (..))
import Control.Arrow
import Control.Monad (guard)
import Control.Monad.Hefty
import Control.Monad.Hefty.NonDet
import Control.Monad.Hefty.State
import Data.Maybe (mapMaybe)
import HeftiaParser

data Mul a = Mul a a
  deriving (Eq, Show)

data Switch = Do | Dont
  deriving (Show, Eq)

data SMul a = S Switch | M (Mul a)
  deriving (Eq, Show)

mulParser :: forall eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef (Mul Int)
mulParser = do
  string "mul("
  a <- signedInteger
  char ','
  b <- signedInteger
  char ')'
  guard (a >= 0 && a < 1000 && b >= 0 && b < 1000)
  pure (Mul a b)

manyMul :: forall eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef [Mul Int]
manyMul =
  (eof @_ @eh >> pure [])
    <|> try @[Mul Int] @String ((:) <$> mulParser <*> manyMul)
    <|> (anySingle >> manyMul)

switchParser :: forall eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef Switch
switchParser =
  (string "do()" >> pure Do) <|> (string "don't()" >> pure Dont)

parseTillDo :: forall eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef [Mul Int]
parseTillDo =
  (eof >> pure [])
    <|> (try (string "do()") >> manyMulSwitch)
    <|> (anySingle >> parseTillDo)

manyMulSwitch :: forall eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef [Mul Int]
manyMulSwitch =
  (eof >> pure [])
    <|> (try (string "don't()") >> parseTillDo)
    <|> try ((:) <$> mulParser <*> manyMulSwitch)
    <|> (anySingle >> manyMulSwitch)

manySMul :: forall eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef [SMul Int]
manySMul =
  (eof >> pure [])
    <|> try @[SMul Int] @String ((:) . S <$> switchParser <*> manySMul)
    <|> try @[SMul Int] @String ((:) . M <$> mulParser <*> manySMul)
    <|> (anySingle >> manySMul)

applyMul :: Mul Int -> Int
applyMul (Mul x y) = x * y

day3 :: IO ()
day3 = do
  input <- readFile "input/input3.txt"
  putStrLn
    . ("day3a: " ++)
    . show 
    . fmap (sum . map applyMul)
    . runPure
    . evalState input
    . runNonDetMaybe
    $ runChooseH (manyMul <* eof)
  putStrLn
    . ("day3b: " ++)
    . show 
    . fmap (sum . map applyMul)
    . runPure
    . evalState input
    . runNonDetMaybe
    $ runChooseH (manyMulSwitch <* eof)
