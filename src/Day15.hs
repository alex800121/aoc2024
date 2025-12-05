module Day15 where

import Control.Monad.Hefty (Eff, Effect, FOEs, runPure, (:>))
import Control.Monad.Hefty.State (State (..), modify, runState)
import Data.Bifunctor
import Data.Kind (Constraint)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (trace)
import MyLib (Direction (..), drawGraph, drawMapWithKeyM, toIndex)
import Paths_AOC2024 (getDataDir)

type Index = (Int, Int)

data GameState = G {_m :: M, _robot :: Index}
  deriving (Show, Eq, Ord)

type M = Map Index Cell

data Cell = Wall | Space | Box Index deriving (Show, Eq, Ord)

type Instructions = [Direction]

readInput :: String -> (GameState, Instructions)
readInput input = (G m r, ins)
  where
    ins =
      mapMaybe
        ( \case
            '>' -> Just East
            '<' -> Just West
            '^' -> Just North
            'v' -> Just South
            _ -> Nothing
        )
        b
    [a, b] = splitOn "\n\n" input
    (r, m) = runPure $ runState undefined $ drawMapWithKeyM f (0, 0) a
    f :: forall s (es :: [Effect]) a. (FOEs es, State Index :> es) => Index -> Char -> Eff es (Index, Maybe Cell)
    f (_, y) '\n' = pure ((0, y + 1), Nothing)
    f (x, y) 'O' = pure ((x + 1, y), Just (Box (x, y)))
    f (x, y) '.' = pure ((x + 1, y), Just Space)
    f (x, y) '@' = do
      modify (const (x, y))
      pure ((x + 1, y), Just Space)
    f (x, y) '#' = pure ((x + 1, y), Just Wall)

moveBox :: Direction -> GameState -> Maybe GameState
moveBox d g@(G m r@(x, y)) = go m [(r', Space)]
  where
    (dx, dy) = toIndex d
    move = bimap (+ dx) (+ dy)
    moveC Space = Space
    moveC (Box i) = Box (move i)
    r' = move r
    go m [] = Just (G m r')
    go m ((i, c) : is) = case m Map.!? i of
      Just Wall -> Nothing
      Just Space -> go (Map.insert i c m) is
      Just b@(Box j) -> go (Map.insert i c $ Map.insert j Space m) (ins (move i, moveC b) $ ins (move j, moveC (m Map.! j)) is)
      Nothing -> error $ show i
    ins (i, c) is = (i, c) : filter ((/= i) . fst) is

calcScore :: M -> Int
calcScore m =
  sum
    . map (\(x, y) -> x + 100 * y)
    . Map.keys
    $ Map.filterWithKey (\k b -> case (k, b) of ((x, _), Box (x', _)) | x' >= x -> True; _ -> False) m

expand :: GameState -> GameState
expand (G m (x, y)) = G m' (2 * x, y)
  where
    m' = Map.foldlWithKey f Map.empty m
    f acc (x, y) (Box _) = Map.insert (2 * x, y) (Box (2 * x + 1, y)) $ Map.insert (2 * x + 1, y) (Box (2 * x, y)) acc
    f acc (x, y) c = Map.insert (2 * x, y) c $ Map.insert (2 * x + 1, y) c acc

day15 :: IO (String, String)
day15 = do
  input <- readInput <$> (readFile . (++ "/input/input15.txt") =<< getDataDir)
  let !finalAnsa =
        show
          . calcScore
          . _m
          $ uncurry (foldl' (\acc x -> fromMaybe acc (moveBox x acc))) input
  let !finalAnsb =
        show
          . calcScore
          . _m
          . uncurry (foldl' (\acc x -> fromMaybe acc (moveBox x acc)))
          $ first expand input
  pure (finalAnsa, finalAnsb)
