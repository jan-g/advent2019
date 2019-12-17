module Day18 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char

import Control.Monad

{-
-}

parse ls = ls
         & head
         & splitOn "-"
         & map read

day18 ls = do
  l <- [1, 2, 3, 4, 5]
  let x = l + 1
  guard $ x `mod` 2 == 0
  return x

{-
-}

day18b ls = "hello world"
