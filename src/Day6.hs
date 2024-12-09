module Day6 where

import Control.Monad.ST (ST, runST)
import Data.Array (Array)
import Data.Array.IArray ( Ix(inRange), Array, (!?), IArray(bounds), elems )
import Data.Array.ST (MArray (newArray, newArray_), STArray, freeze, runSTArray, writeArray, getElems, modifyArray')
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.List (elemIndex, find, findIndex)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word8)
import MyLib (Direction (..), Parser, drawArray, toIndex)
import Paths_AOC2024 (getDataDir)
import GHC.Arr (newSTArray)

type Index = (Int, Int)

type Guard = (Index, Direction)

-- data Cell = Space | Obstacle | Stepped Word8 deriving (Show, Eq, Ord)
data Cell = Space | Obstacle deriving (Show, Eq, Ord)

type A = Array Index Cell
inputParser :: String -> (Guard, A)
inputParser s = runST $ do
  arr <- newArray ((0, 0), (length x - 1, length xs)) Space :: ST s (STArray s Index Cell)
  (,) <$> go arr undefined 0 0 x xs <*> freeze arr
  where
    x : xs = lines s
    go arr g x y [] [] = pure g
    go arr g x y [] (l : xs) = go arr g 0 (succ y) l xs
    go arr g x y (c : ys) xs = case c of
      '#' -> writeArray arr (x, y) Obstacle >> go arr g (succ x) y ys xs
      'v' -> go arr ((x, y), South) (succ x) y ys xs
      '^' -> go arr ((x, y), North) (succ x) y ys xs
      '<' -> go arr ((x, y), West) (succ x) y ys xs
      '>' -> go arr ((x, y), East) (succ x) y ys xs
      _ -> go arr g (succ x) y ys xs

walk :: A -> Guard -> Maybe (Int, Int)
walk m = go Nothing (0, Map.empty)
  where
    testRange = inRange (bounds m)
    go testing (n, s) (i, d)
      -- repeated
      | maybe False (flip (testBit @Word8) (fromEnum d)) (s Map.!? i) = Nothing
      -- step out of bound -> return Just (if testing -> undefined; else -> answer)
      | Just (iTurn, d') <- next,
        not (testRange iTurn) =
          Just (if isJust testing then (undefined, undefined) else (n, Map.size s'))
      -- in bound, not testing, next cell not travelled -> test next cell and continue
      | Just (iTurn, d') <- next,
        Nothing <- testing,
        iTurn `Map.notMember` s' =
          go testing (if isJust (go (Just iTurn) (undefined, s) (i, d)) then n else n + 1, s') (iTurn, d')
      | Just (iTurn, d') <- next = go testing (n, s') (iTurn, d')
      where
        s' = Map.insertWith (.|.) i (bit $ fromEnum d) s
        next =
          find (\(i, _) -> Just Obstacle /= m !? i && Just i /= testing)
            . map (\d -> (bimap (+ fst i) (+ snd i) (toIndex d), d))
            . take 4
            $ iterate succ d

-- type A = Map Index Cell
-- inputParser :: String -> (Guard, A)
-- inputParser s = go Map.empty undefined 0 0 x xs
--   where
--     x : xs = lines s
--     go arr g x y [] [] = (g, arr)
--     go arr g x y [] (l : xs) = go arr g 0 (succ y) l xs
--     go arr g x y (c : ys) xs = case c of
--       '#' -> go arr0 g (succ x) y ys xs
--       'v' -> go arr1 ((x, y), South) (succ x) y ys xs
--       '^' -> go arr1 ((x, y), North) (succ x) y ys xs
--       '<' -> go arr1 ((x, y), West) (succ x) y ys xs
--       '>' -> go arr1 ((x, y), East) (succ x) y ys xs
--       _ -> go arr1 g (succ x) y ys xs
--       where
--         arr0 = Map.insert (x, y) Obstacle arr
--         arr1 = Map.insert (x, y) Space arr
-- walk :: A -> Guard -> Maybe (Int, Int)
-- walk m = go Nothing (0, Map.empty)
--   where
--     go :: Maybe Index -> (Int, Map Index Word8) -> Guard -> Maybe (Int, Int)
--     go testing (n, s) (i, d)
--       -- repeated
--       | maybe False (`testBit` fromEnum d) (s Map.!? i) = Nothing
--       -- step out of bound -> return Just (if testing -> undefined; else -> answer)
--       | Just (iTurn, d') <- next,
--         iTurn `Map.notMember` m =
--           Just (if isJust testing then (undefined, undefined) else (n, Map.size s'))
--       -- in bound, not testing, next cell not travelled -> test next cell and continue
--       | Just (iTurn, d') <- next,
--         isNothing testing,
--         iTurn `Map.notMember` s' =
--           go testing (if isJust (go (Just iTurn) (undefined, s) (i, d)) then n else n + 1, s') (iTurn, d')
--       | Just (iTurn, d') <- next = go testing (n, s') (iTurn, d')
--       where
--         s' = Map.insertWith (.|.) i (bit $ fromEnum d) s
--         next =
--           find (\(i, _) -> Just Obstacle /= m Map.!? i && Just i /= testing)
--             . map (\d -> (bimap (+ fst i) (+ snd i) (toIndex d), d))
--             . take 4
--             $ iterate succ d

day6 :: IO ()
day6 = do
  -- input <- readFile . (++ "/input/test6.txt") =<< getDataDir
  input <- readFile . (++ "/input/input6.txt") =<< getDataDir
  let (g, a) = inputParser input
      walked = walk a g
  putStrLn
    . ("day6a: " ++)
    . show
    $ snd <$> walked
  putStrLn
    . ("day6b: " ++)
    . show
    $ fst <$> walked
