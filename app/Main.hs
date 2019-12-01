module Main where

import System.Environment

import Lib
import Day1

main :: IO ()
main = do
  args <- getArgs
  let action = case args !! 0 of
                 "day1" -> day1
                 "day1b" -> day1b
      source = args !! 1
  ls <- loadLines source
  action ls

