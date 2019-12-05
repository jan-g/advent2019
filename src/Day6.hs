module Day6 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char

{-
-}

parse ls = ls
         & head
         & splitOn "-"
         & map read

day6 ls = "hello world"

{-
-}

day6b ls = "hello world"
