module Intcode (
  Addr, Prog(..), I(..), A(..),
  parse, toProg,
  run,
  peek, poke, addRelative, getRelative,
  dump
) where


import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Map.Lazy as Map
import Lib


type Addr = Integer
data Prog = Prog (Map.Map Addr Integer) Addr

parse ns = ns
         & splitOn ","
         & map (read :: String -> Integer)
         & toProg

toProg :: [Integer] -> Prog
toProg ns = Prog (Map.fromAscList $ [0..] `zip` ns) 0


poke (Prog a o) addr value = Prog (Map.insert addr value a) o
peek (Prog a o) addr = Map.findWithDefault 0 addr a
addRelative (Prog a o) offset = Prog a (o + offset)
getRelative (Prog a o) = o


run prog inputs = run0 0 prog inputs []

run0 pc prog inputs outputs =
  let op = peek prog pc `mod` 100
  in case op of
        99 -> (prog, outputs)
        1 -> run0 (pc + 4) (apply (+)) inputs outputs
        2 -> run0 (pc + 4) (apply (*)) inputs outputs
        3 -> run0 (pc + 2) (poke prog (addr 1) (head inputs)) (tail inputs) outputs
        4 -> run0 (pc + 2) prog inputs (outputs ++ [arg 1])
        5 -> run0 (if arg 1 /= 0 then arg 2 else pc + 3) prog inputs outputs
        6 -> run0 (if arg 1 == 0 then arg 2 else pc + 3) prog inputs outputs
        7 -> run0 (pc + 4) (apply (\a b -> if a < b then 1 else 0)) inputs outputs
        8 -> run0 (pc + 4) (apply (\a b -> if a == b then 1 else 0)) inputs outputs
        9 -> run0 (pc + 2) (addRelative prog (arg 1)) inputs outputs
        _ -> error ("not possible " ++ (show pc) ++ " " ++ (show op))
  where
    arg n =
      let inst = peek prog pc
          val = peek prog (pc + n)
          mode = inst `div` (10 ^ (n + 1)) `mod` 10
      in  case mode of
          0 -> peek prog val
          1 -> val
          2 -> peek prog (val + getRelative prog)  
    addr n =
      let inst = peek prog pc
          val = peek prog (pc + n)
          mode = inst `div` (10 ^ (n + 1)) `mod` 10
      in  case mode of
          0 -> val
          2 -> val + getRelative prog
    apply op =
      let a1 = arg 1
          a2 = arg 2
          to = addr 3
       in poke prog to (op a1 a2)


data I = Add A A A
       | Mul A A A
       | Input A 
       | Output A
       | JTrue A A
       | JFalse A A 
       | Lt A A A 
       | Eq A A A
       | AddOffs A
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
  show (AddOffs a)  = "@ += " ++ (show a)
  show (Halt)       = "Halt"
  show (Value v)    = "Value " ++ (show v)

data A = Ind Integer
       | Abs Integer
       | Rel Integer
  deriving (Eq)

instance Show A where
  show (Ind x) = "r" ++ (show x)
  show (Abs x) = (show x)
  show (Rel x) = "r(@ + " ++ (show x) ++ ")"

dump (Prog prog offs) o =
  decode (Map.toAscList prog & drop o) []
  where
    decode [] out = out
    decode ((addr, v):(_, a):(_, b):(_, c):ps) d
      | v `mod` 100 == 1 = decode ps (d ++ [(addr, [v, a, b, c], Add a' b' c')])
      | v `mod` 100 == 2 = decode ps (d ++ [(addr, [v, a, b, c], Mul a' b' c')])
      | v `mod` 100 == 7 = decode ps (d ++ [(addr, [v, a, b, c], Lt a' b' c')])
      | v `mod` 100 == 8 = decode ps (d ++ [(addr, [v, a, b, c], Eq a' b' c')])
      where a' = toAddr a (v `div` 100)
            b' = toAddr b (v `div` 1000)
            c' = toAddr c (v `div` 10000)
            toAddr x v
               | v `mod` 10 == 0 = Ind x
               | v `mod` 10 == 1 = Abs x
               | v `mod` 10 == 2 = Rel x
    decode ((addr, v):(_, a):(_, b):ps) d
      | v `mod` 100 == 5 = decode ps (d ++ [(addr, [v, a, b], JTrue a' b')])
      | v `mod` 100 == 6 = decode ps (d ++ [(addr, [v, a, b], JFalse a' b')])
      where a' = toAddr a (v `div` 100)
            b' = toAddr b (v `div` 1000)
            toAddr x v
               | v `mod` 10 == 0 = Ind x
               | v `mod` 10 == 1 = Abs x
               | v `mod` 10 == 2 = Rel x
    decode ((addr, v):(_, a):ps) d
      | v `mod` 100 == 3 = decode ps (d ++ [(addr, [v, a], Input a')])
      | v `mod` 100 == 4 = decode ps (d ++ [(addr, [v, a], Output a')])
      | v `mod` 100 == 9 = decode ps (d ++ [(addr, [v, a], AddOffs a')])
      where a' = toAddr a (v `div` 100)
            toAddr x v
               | v `mod` 10 == 0 = Ind x
               | v `mod` 10 == 1 = Abs x
               | v `mod` 10 == 2 = Rel x
    decode ((addr, v):ps) d
      | v `mod` 100 == 99 = decode ps (d ++ [(addr, [v], Halt)])
      | otherwise         = decode ps (d ++ [(addr, [v], Value v)])
