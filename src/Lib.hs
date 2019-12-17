module Lib
    ( loadLines
    , makeArray
    , natParser
    , intParser
    , drawMapWith
    ) where

import Data.Array
import Data.Char
import Text.ParserCombinators.ReadP
import qualified Data.Map as Map
import qualified Data.Set as Set

loadLines fn = do
  contents <- readFile fn
  return (lines contents)

makeArray :: [a] -> Array Int a
makeArray ns = listArray (0, length ns - 1) ns

natParser :: ReadP Integer
natParser = do
  digits <- munch1 isDigit
  return $ read digits

intParser :: ReadP Integer
intParser = do
  (do
    char '-'
    i <- natParser
    return $ -i) <++ natParser

drawMapWith :: (Ord a, Enum a) => ((a, a) -> Maybe b -> c) -> Map.Map (a, a) b -> [[c]]
drawMapWith f m =
  let coords = Map.keysSet m
      xs = Set.map fst coords
      ys = Set.map snd coords
      (x0, x1) = (Set.findMin xs, Set.findMax xs)
      (y0, y1) = (Set.findMin ys, Set.findMax ys)
  in  [[f (x, y) (Map.lookup (x, y) m) | x <- [x0..x1]] | y <- [y0..y1]]
