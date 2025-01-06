module Day12 where

import Data.Array.IArray qualified as A
import Data.Array.Unboxed (Array)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.Graph qualified as G
import Data.Set (Set)
import Data.Set qualified as Set
import MyLib (drawArray, drawMap, toIndex)
import Paths_AOC2024 (getDataDir)

type Index = (Int, Int)

aToG a =
  G.graphFromEdges
    [ (length xs, i, xs)
      | (i@(x, y), c) <- A.assocs a,
        let xs =
              filter ((== Just c) . (a A.!?)) $
                map (bimap (+ x) (+ y) . toIndex) [minBound .. maxBound]
    ]

corners s = length openC + length closeC
  where
    f d (x, y) = bimap (+ x) (+ y) $ toIndex d
    l = Set.toList s
    ds = [minBound .. maxBound]
    openC =
      [ ()
        | a <- l,
          d <- ds,
          open (f d a),
          open (f (succ d) a)
      ]
    closeC =
      [ ()
        | a <- l,
          d <- ds,
          open (f d a),
          close (f (succ d) a),
          close (f (succ d) $ f d a)
      ]
    open = (`Set.notMember` s)
    close = (`Set.member` s)

day12 :: IO ()
day12 = do
  (input, vToN, _) <- aToG . drawArray @Array . lines <$> (readFile . (++ "/input/input12.txt") =<< getDataDir)
  putStrLn
    . ("day12a: " ++)
    . show
    . sum
    . map (\x -> length x * (length x * 4 - sum (fmap (\v -> let (n, _, _) = vToN v in n) x)))
    $ G.scc input
  putStrLn
    . ("day12b: " ++)
    . show
    . sum
    . map (\x -> length x * corners (Set.fromList $ map (\v -> let (_, k, _) = vToN v in k) (toList x)))
    $ G.scc input
