module Main where

import System.Environment

import Lib
import Day1
import Day2

main :: IO ()
main = do
  args <- getArgs
  let action = case args !! 0 of
                 "day1" -> day1
                 "day1b" -> day1b
                 "day2" -> day2
                 "day2b" -> day2b
      source = args !! 1
  ls <- loadLines source
  putStrLn (show $ action ls)

