module Lib
    ( loadLines
    , makeArray
    ) where

import Data.Array

loadLines fn = do
  contents <- readFile fn
  return (lines contents)

makeArray :: [a] -> Array Int a
makeArray ns = listArray (0, length ns - 1) ns

