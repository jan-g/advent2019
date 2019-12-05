module Intcode where


import Data.Function ((&))
import Data.List.Split
import Data.Array
import Lib


type Prog = Array Integer Integer

parse ns = ns
         & splitOn ","
         & map (read :: String -> Integer)
         & toProg

toProg :: [Integer] -> Prog
toProg ns = listArray (0, length ns - 1 & toInteger) ns


set prog addr value = prog // [(addr, value)]
get prog addr = prog ! addr


run prog inputs = run0 0 prog inputs []

run0 pc prog inputs outputs =
  let op = get prog pc `mod` 100
   in case op of
        99 -> (prog, outputs)
        1 -> run0 (pc + 4) (apply (+)) inputs outputs
        2 -> run0 (pc + 4) (apply (*)) inputs outputs
        3 -> run0 (pc + 2) (set prog (get prog (pc + 1)) (head inputs)) (tail inputs) outputs
        4 -> run0 (pc + 2) prog inputs (outputs ++ [arg 1])
        5 -> run0 (if arg 1 /= 0 then arg 2 else pc + 3) prog inputs outputs
        6 -> run0 (if arg 1 == 0 then arg 2 else pc + 3) prog inputs outputs
        7 -> run0 (pc + 4) (apply (\a b -> if a < b then 1 else 0)) inputs outputs
        8 -> run0 (pc + 4) (apply (\a b -> if a == b then 1 else 0)) inputs outputs
        _ -> error ("not possible " ++ (show pc) ++ " " ++ (show op))
  where
    arg n =
      let inst = get prog pc
          val = get prog (pc + n)
          mode = inst `div` (10 ^ (n + 1)) `mod` 10
      in  case mode of
          0 -> get prog val
          1 -> val
    apply op =
      let a1 = arg 1
          a2 = arg 2
          to = get prog (pc + 3)
       in set prog to (op a1 a2)
