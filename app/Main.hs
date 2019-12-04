module Main where

import System.Environment

import Lib
import Day1
import Day2
import Day3
import Day4

main :: IO ()
main = do
  args <- getArgs
  let action = case args !! 0 of
                 "day1" -> show . day1
                 "day1b" -> show . day1b
                 "day2" -> show . day2
                 "day2b" -> show . day2b
                 "day3" -> show . day3
                 "day3b" -> show . day3b
                 "day4" -> show . day4
                 "day4b" -> show . day4b
      source = args !! 1
  ls <- loadLines source
  putStrLn (action ls)

