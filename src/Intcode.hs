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


data I = Add A A A
       | Mul A A A
       | Input A 
       | Output A
       | JTrue A A
       | JFalse A A 
       | Lt A A A 
       | Eq A A A
       | Halt
       | Value Integer
  deriving (Eq)

instance Show I where
  show (Add a b c)  = (show c) ++ " = " ++ (show a) ++ " + " ++ (show b)
  show (Mul a b c)  = (show c) ++ " = " ++ (show a) ++ " * " ++ (show b)
  show (Input a)    = "Input " ++ (show a)
  show (Output a)   = "Output " ++ (show a)
  show (JTrue a b)  = "If " ++ (show a) ++ " goto " ++ (show b)
  show (JFalse a b) = "If Not " ++ (show a) ++ " goto " ++ (show b)
  show (Lt a b c)   = (show c) ++ " = " ++ (show a) ++ " < " ++ (show b)
  show (Eq a b c)   = (show c) ++ " = " ++ (show a) ++ " == " ++ (show b)
  show (Halt)       = "Halt"
  show (Value v)    = "Value " ++ (show v)

data A = Ind Integer
       | Abs Integer
  deriving (Eq)

instance Show A where
  show (Ind x) = "r" ++ (show x)
  show (Abs x) = (show x)

dump prog =
  decode (Data.Array.assocs prog) []
  where
    decode [] out = out
    decode ((addr, v):(_, a):(_, b):(_, c):ps) d
      | v `mod` 100 == 1 = decode ps (d ++ [(addr, Add a' b' c')])
      | v `mod` 100 == 2 = decode ps (d ++ [(addr, Mul a' b' c')])
      | v `mod` 100 == 7 = decode ps (d ++ [(addr, Lt a' b' c')])
      | v `mod` 100 == 8 = decode ps (d ++ [(addr, Eq a' b' c')])
      where a' = toAddr a (v `div` 100)
            b' = toAddr b (v `div` 1000)
            c' = toAddr c (v `div` 10000)
            toAddr x v
               | v `mod` 10 == 0 = Ind x
               | v `mod` 10 == 1 = Abs x
    decode ((addr, v):(_, a):(_, b):ps) d
      | v `mod` 100 == 5 = decode ps (d ++ [(addr, JTrue a' b')])
      | v `mod` 100 == 6 = decode ps (d ++ [(addr, JFalse a' b')])
      where a' = toAddr a (v `div` 100)
            b' = toAddr b (v `div` 1000)
            toAddr x v
               | v `mod` 10 == 0 = Ind x
               | v `mod` 10 == 1 = Abs x
    decode ((addr, v):(_, a):ps) d
      | v `mod` 100 == 3 = decode ps (d ++ [(addr, Input a')])
      | v `mod` 100 == 4 = decode ps (d ++ [(addr, Output a')])
      where a' = toAddr a (v `div` 100)
            toAddr x v
               | v `mod` 10 == 0 = Ind x
               | v `mod` 10 == 1 = Abs x
    decode ((addr, v):ps) d
      | v `mod` 100 == 99 = decode ps (d ++ [(addr, Halt)])
      | otherwise         = decode ps (d ++ [(addr, Value v)])
