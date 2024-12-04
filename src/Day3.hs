{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Day3 where

import Control.Applicative (Alternative (..))
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

manySMul :: forall eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef [SMul Int]
manySMul =
  (eof >> pure [])
    <|> try @[SMul Int] @String ((:) . S <$> switchParser <*> manySMul)
    <|> try @[SMul Int] @String ((:) . M <$> mulParser <*> manySMul)
    <|> (anySingle >> manySMul)

applyMul :: Mul Int -> Int
applyMul (Mul x y) = x * y

filterSMul :: [SMul a] -> [Mul a]
filterSMul [] = []
filterSMul (M x : xs) = x : filterSMul xs
filterSMul (S Do : xs) = filterSMul xs
filterSMul (S Dont : xs) = f xs
  where
    f [] = []
    f (S Do : xs) = filterSMul xs
    f (_ : xs) = f xs

day3 :: IO ()
day3 = do
  input <- readFile "input/input3.txt"
  putStrLn
    . ("day3a: " ++)
    . show @(Maybe [Mul Int])
    $ runPure $ evalState input $ runNonDet $ runChooseH (manyMul <* eof)
  -- putStrLn
  --   . ("day3b: " ++)
  --   . show
  --   $ (sum . fmap applyMul . filterSMul <$> runPure $ evalState input $ runNonDet $ runChooseH (manySMul <* eof))
  print @(Maybe _)
    . runPure
    . runNonDet
    . evalState "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    $ runChooseH manySMul
