module Day22 where

import Data.Bits (Bits (..))
import Data.List (foldl')
import Data.List.Split (divvy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.IntMap.Strict qualified as IM
import Paths_AOC2024 (getDataDir)

mixPrune f = (.&. (16777216 - 1)) . (\x -> xor x (f x)) :: Int -> Int

step = mixPrune (`shiftL` 11) . mixPrune (`shiftR` 5) . mixPrune (`shiftL` 6)

toIM = foldr (\x acc -> acc * 32 + (x + 9)) 0

day22 :: IO ()
day22 = do
  input <- map (read @Int) . lines <$> (readFile . (++ "/input/input22.txt") =<< getDataDir)
  let ansA = map (take 2001 . iterate step) input
      ansB =
        IM.unionsWith (+) $
          map
            ( IM.fromListWith (const id)
                . ( zip
                      <$> ( map toIM
                              . divvy 4 1
                              . (zipWith subtract <*> tail)
                          )
                      <*> drop 4
                  )
                . map (`mod` 10)
            )
            ansA
  putStrLn
    . ("day22a: " ++)
    . show
    . sum
    $ map last ansA
  putStrLn
    . ("day22b: " ++)
    . show
    $ maximum ansB
