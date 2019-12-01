module Lib
    ( loadLines
    ) where

loadLines fn = do
  contents <- readFile fn
  return (lines contents)
