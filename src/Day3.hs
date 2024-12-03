module Day3 where

import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import MyLib (Parser, signedInteger)
import Text.Megaparsec
import Text.Megaparsec.Char

data Mul a = Mul a a
  deriving (Show)

data Switch = Do | Dont
  deriving (Show)

data SMul a = S Switch | M (Mul a)
  deriving (Show)

mulParser :: Parser (Mul Int)
mulParser = do
  string "mul("
  a <- signedInteger
  char ','
  b <- signedInteger
  char ')'
  guard (a >= 0 && a < 1000 && b >= 0 && b < 1000)
  pure (Mul a b)

manyMul :: Parser [Mul Int]
manyMul =
  (eof >> pure [])
    <|> try ((:) <$> mulParser <*> manyMul)
    <|> (anySingle >> manyMul)

switchParser :: Parser Switch
switchParser =
  (string "do()" >> pure Do) <|> (string "don't()" >> pure Dont)

manySMul :: Parser [SMul Int]
manySMul =
  (eof >> pure [])
    <|> try ((:) . S <$> switchParser <*> manySMul)
    <|> try ((:) . M <$> mulParser <*> manySMul)
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
    . show
    $ (sum . fmap applyMul <$> parseMaybe manyMul input)
  putStrLn
    . ("day3b: " ++)
    . show
    $ (sum . fmap applyMul . filterSMul <$> parseMaybe manySMul input)
