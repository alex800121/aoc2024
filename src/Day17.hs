module Day17 where

import Control.Monad (guard)
import Data.Bits (Bits (..))
import Data.Vector.Mutable qualified as M
import Data.Vector.Strict qualified as U
import Debug.Trace (traceM, traceShow)
import MyLib (Parser)
import Paths_AOC2024 (getDataDir)
import Data.List (intercalate)

-- test:
-- Register A: 2024
-- Register B: 0
-- Register C: 0
--
-- Program: 0,3,5,4,3,0
ins' :: Ins
ins' = U.fromList [0, 3, 5, 4, 3, 0]

input' = G 0 (R 2024 0 0) []

-- input:
-- Register A: 33024962
-- Register B: 0
-- Register C: 0
--
-- Program: 2,4,1,3,7,5,1,5,0,3,4,2,5,5,3,0
{--
 - a = x, b = 0, c = 0
 - 0. 2 4 => b <- a and b111 => b <- x and b111
 - 2. 1 3 => b <- b xor b011 => b <- (x and b111) xor b011
 - 4. 7 5 => c <- a shiftR b => c <- x shiftR ((x and b111) xor b011)
 - 6. 1 5 => b <- b xor b101 => b <- ((x and b111) xor b011) xor b101
 - 8. 0 3 => a <- a shiftR 3 => a <- x shiftR 3
 - 10. 4 2 => b <- b xor c => b <- (((x and b111) xor b011) xor b101) xor (x shiftR ((x and b111) xor b011))
 - 12. 5 5 => b and b111 => output b
 - 14. 3 0 => if a == 0 then end, else a = x shiftR 3 and restart
 --}
ins :: Ins
ins = U.fromList [2, 4, 1, 3, 7, 5, 1, 5, 0, 3, 4, 2, 5, 5, 3, 0]

input = G 0 (R 33024962 0 0) []

type Ins = U.Vector Integer

data GameState a = G {_pos :: Int, _reg :: Reg a, _out :: [a]}
  deriving (Show)

data Reg a = R {_a :: a, _b :: a, _c :: a}
  deriving (Show)

solveB :: Ins -> [Integer]
solveB ins = go (reverse $ U.toList ins) 0
  where
    go [] a = pure a
    go (n : ns) a = do
      c <- [0 .. 7]
      let a8 = a * 8 + c
      guard $ n == last (_out (run ins (G 0 (R a8 0 0) [])))
      go ns a8

step :: Ins -> GameState Integer -> Maybe (GameState Integer)
step ins (G pos reg out) = f <$> ((,) <$> (ins U.!? pos) <*> (ins U.!? (pos + 1)))
  where
    combo a | a >= 0 && a <= 3 = a
    combo 4 = _a reg
    combo 5 = _b reg
    combo 6 = _c reg
    combo a = error $ show a ++ " is not a valid operand"
    f (0, c) = G (pos + 2) (reg {_a = _a reg `shiftR` fromIntegral (combo c)}) out
    f (1, c) = G (pos + 2) (reg {_b = _b reg `xor` c}) out
    f (2, c) = G (pos + 2) (reg {_b = combo c .&. 7}) out
    f (3, c)
      | _a reg == 0 = G (pos + 2) reg out
      | otherwise = G (fromIntegral c) reg out
    f (4, c) = G (pos + 2) (reg {_b = _b reg `xor` _c reg}) out
    f (5, c) = G (pos + 2) reg (o : out)
      where
        o = combo c .&. 7
    f (6, c) = G (pos + 2) (reg {_b = _a reg `shiftR` fromIntegral (combo c)}) out
    f (7, c) = G (pos + 2) (reg {_c = _a reg `shiftR` fromIntegral (combo c)}) out

run ins g = maybe g (run ins) (step ins g)

day17 :: IO ()
day17 = do
  putStrLn
    . ("day17a: " ++)
    . intercalate ","
    . map show
    . reverse
    . _out 
    $ run ins input
  putStrLn
    . ("day17b: " ++)
    . show
    . minimum
    $ solveB ins
