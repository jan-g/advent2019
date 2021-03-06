module Main where

import System.Environment
import Control.Monad (forM_)

import Lib
import qualified Intcode
import qualified IntcodeStepIO as IS
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24

main :: IO ()
main = do
  args <- getArgs
  let source = args !! 1
  ls <- loadLines source

  if head args == "dump"
  then
    dump ls (read $ args !! 2)

  else if head args == "debug"
  then
    debug ls

  else if head args == "day17-dump"
  then do
    day17Dump ls
    return ()

  else if head args == "day18b"
  then do
    ans <- day18b ls
    print ans
    return ()

  else if head args == "day18c"
  then do
    print "day 18c"
    ans <- day18c ls
    print ans
    return ()

  else if head args == "day21"
  then do
    print "day 21"
    ans <- day21 ls
    print ans
    return ()

  else if head args == "day21b"
  then do
    print "day 21b"
    ans <- day21b ls
    print ans
    return ()

  else if head args == "day22"
  then do
    print "day 22"
    ans <- day22 ls
    print ans
    return ()

  else if head args == "day23"
  then do
    print "day 23"
    ans <- day23 ls
    print ans
    return ()

  else if head args == "day23b"
  then do
    print "day 23b"
    ans <- day23b ls
    print ans
    return ()


  else do
    let action = case args !! 0 of
                   "day1" -> show . day1
                   "day1b" -> show . day1b
                   "day2" -> show . day2
                   "day2b" -> show . day2b
                   "day3" -> show . day3
                   "day3b" -> show . day3b
                   "day4" -> show . day4
                   "day4b" -> show . day4b
                   "day5" -> show . day5
                   "day5b" -> show . day5b
                   "day6" -> show . day6
                   "day6b" -> show . day6b
                   "day7" -> show . day7
                   "day7b" -> show . day7b
                   "day8" -> show . day8
                   "day8b" -> day8b
                   "day9" -> show . day9
                   "day9b" -> show . day9b
                   "day10" -> show . day10
                   "day10b" -> show . day10b
                   "day11" -> show . day11
                   "day11b" -> day11b
                   "day12" -> show . day12
                   "day12b" -> show . day12b
                   "day13" -> show . day13
                   "day13b" -> show . day13b
                   "day14" -> show . day14
                   "day14b" -> show . day14b
                   "day15" -> show . day15
                   "day15b" -> \ls ->
                     let (_, t, s) = day15b $ ls
                     in  s ++ "\n" ++ (show t)
                   "day16" -> show . day16
                   "day16b" -> show . day16b
                   "day17" -> show . day17
                   "day17b" -> \ls ->
                     let (a, b, c, r,
                          output, answer) = day17b ls :: (String, String, String, String, String, Integer)
                     in  "\nA=" ++ a ++ "\nB=" ++ b ++ "\nC=" ++ c ++
                         "\nRoute=" ++ r ++
                         "\n" ++ output ++
                         "\n\nAnswer=" ++ show answer
                   "day18" -> show . day18
                   "day18b" -> \ls -> "hello world"
                   "day19" -> \ls ->
                     let (grid, count) = day19 ls
                     in  grid ++
                         "\n\nCount=" ++ show count
                   "day19b" -> \ls ->
                     let (grid, count, coords, scan, answer) = day19b ls
                     in  grid
                         ++ "\n\nCount=" ++ (show count)
                         ++ "\ncoords = " ++ (show coords)
                         ++ "\nscan = " ++ (show scan)
                         ++ "\answer = " ++ (show answer)

                   "day20" -> \ls ->
                     let (maze, ps, start, end, len, path)
                           = day20 ls
                     in  show maze ++
                         "\n" ++ (show ps) ++
                         "\n" ++ (show len)

                   "day20b" -> \ls ->
                     let (maze, ps, start, end, len, path)
                           = day20b ls
                     in  show maze ++
                         "\n" ++ (show ps) ++
                         "\n" ++ (show len)

                   "day21" ->  \ls -> "hello world"
                   "day21b" -> \ls -> "hello world"
--                   "day22" -> show . day22
                   "day22b" -> show . day22b
--                   "day23" -> show . day23
--                   "day23b" -> show . day23b
                   "day24" -> \ls ->
                     let (ans, result) = day24 ls
                     in  ans ++ "\n" ++ show result
                   "day24b" -> show . day24b
    putStrLn (action ls)

dump :: [String] -> Int -> IO ()
dump ls offs = do
  let prog = (Intcode.parse . head) ls
      dis = Intcode.dump prog offs
  forM_ dis $ \(addr, ints, instr) -> do
    putStrLn ((show addr) ++ "\t" ++ (show instr) ++ "\t\t" ++ (show ints))


day17Dump :: [String] -> IO ()
day17Dump ls = do
  let prog = (Intcode.parse . head) ls
      prog' = Intcode.poke prog 0 2
  Left prog'' <- IS.runIO (IS.runUntil 51) prog' []
  return ()


debug ls = do
  let prog = Intcode.parse $ head ls
  IS.debugger prog 0 []
