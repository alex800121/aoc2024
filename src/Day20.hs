module Day20 where

import Data.Array.IArray qualified as A
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (..))
import Data.List (foldl', sort, tails)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import MyLib (drawArray, toIndex)
import Paths_AOC2024 (getDataDir)
import Data.Function (on)

type Index = (Int, Int)

readInput s = (start, end, path)
  where
    ss = drawArray $ lines s :: UArray Index Char
    start = head [p | (p, 'S') <- A.assocs ss]
    end = head [p | (p, 'E') <- A.assocs ss]
    path = Map.fromList $ go Set.empty 0 start
    go travelled n s
      | s == end = [(end, n)]
      | otherwise = (s, n) : go t' (n + 1) s'
      where
        t' = Set.insert s travelled
        s' =
          head $
            [ i
              | x <- [minBound .. maxBound],
                let i = bimap (+ fst s) (+ snd s) (toIndex x),
                i `Set.notMember` t',
                maybe False (/= '#') (ss A.!? i)
            ]

shortCuts path limit target (i, a) =
  length
    [ ()
      | xi <- [negate limit .. limit],
        yi <- [negate limit .. limit],
        let l = abs xi + abs yi,
        l <= limit,
        l >= 2,
        let j = (fst i + xi, snd i + yi),
        Just b <- [path Map.!? j],
        b - a - l >= target
    ]
-- readInput s = (start, end, path)
--   where
--     ss = drawArray $ lines s :: UArray Index Char
--     start = head [p | (p, 'S') <- A.assocs ss]
--     end = head [p | (p, 'E') <- A.assocs ss]
--     path = go Set.empty 0 start
--     go travelled n s
--       | s == end = [(end, n)]
--       | otherwise = (s, n) : go t' (n + 1) s'
--       where
--         t' = Set.insert s travelled
--         s' =
--           head $
--             [ i
--               | x <- [minBound .. maxBound],
--                 let i = bimap (+ fst s) (+ snd s) (toIndex x),
--                 i `Set.notMember` t',
--                 maybe False (/= '#') (ss A.!? i)
--             ]
--
-- shortCuts path limit target =
--   length
--     [ ()
--       | (start, n0) : rest <- tails path,
--         (end, n1) <- drop target rest,
--         let l = uncurry ((+) `on` abs) $ bimap (subtract (fst start)) (subtract (snd start)) end,
--         l <= limit,
--         l >= 2,
--         n1 - n0 - l >= target
--     ]

day20 :: IO ()
day20 = do
  (start, end, path) <- readInput <$> (readFile . (++ "/input/input20.txt") =<< getDataDir)
  let pathList = Map.toList path
  putStrLn
    . ("day20a: " ++)
    . show
    . sum
    $ map (shortCuts path 2 100) pathList
  putStrLn
    . ("day20b: " ++)
    . show
    . sum
    $ map (shortCuts path 20 100) pathList
  -- print $ shortCuts path 2 100
  -- print $ shortCuts path 20 100
