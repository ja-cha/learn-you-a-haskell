module MainLib
  ( someFunc
  ) where

import           Utils (quicksort)

someFunc :: IO ()
someFunc =
  putStrLn $
  quicksort [45, 12, 29, 5, 68, 23, 53, 6, 345, 624, 56, 7, 1, 2] >>=
  (' ' :) . show
