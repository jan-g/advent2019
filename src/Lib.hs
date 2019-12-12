module Lib
    ( loadLines
    , makeArray
    , natParser
    , intParser
    ) where

import Data.Array
import Data.Char
import Text.ParserCombinators.ReadP

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
