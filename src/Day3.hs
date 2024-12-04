module Day3 where

import Control.Monad (void, guard)
import Data.Maybe (mapMaybe)
import MyLib (Parser, signedInteger)
import Text.Megaparsec
import Text.Megaparsec.Char

data Mul a = Mul a a
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

parseTillDo :: Parser [Mul Int]
parseTillDo =
  (eof >> pure [])
    <|> (try (string "do()") >> manyMulSwitch)
    <|> (anySingle >> parseTillDo)

manyMulSwitch :: Parser [Mul Int]
manyMulSwitch =
  (eof >> pure [])
    <|> (try (string "don't()") >> parseTillDo)
    <|> try ((:) <$> mulParser <*> manyMulSwitch)
    <|> (anySingle >> manyMulSwitch)

applyMul :: Mul Int -> Int
applyMul (Mul x y) = x * y

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
    $ (sum . fmap applyMul <$> parseMaybe manyMulSwitch input)
