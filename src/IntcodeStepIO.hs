module IntcodeStepIO where

import Intcode (Addr, Prog(..), I(..), A(..), parse, toProg, peek, poke, addRelative, getRelative, dump)

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Map.Lazy as Map
import Lib
import Control.Monad.Extra (forM_)


type Tracer r = Prog -> Addr -> IO (Maybe r)

runIO :: Tracer r -> Prog -> [Integer] -> IO (Either r (Prog, [Integer], Bool))
runIO tracer prog inputs = run0 tracer 0 prog inputs []

run0 :: Tracer r -> Addr -> Prog -> [Integer] -> [Integer] -> IO (Either r (Prog, [Integer], Bool))
run0 tracer pc prog inputs outputs = do
  trace <- tracer prog pc
  case trace of
    Just r -> return $ Left r
    Nothing ->
      let op = peek prog pc `mod` 100
      in case op of
            99 -> return $ Right (prog, outputs, False)
            1 -> run0 tracer (pc + 4) (apply (+)) inputs outputs
            2 -> run0 tracer (pc + 4) (apply (*)) inputs outputs
            3 -> if inputs /= []
                 then run0 tracer (pc + 2) (poke prog (addr 1) (head inputs)) (tail inputs) outputs
                 else return $ Right (prog, outputs, True)
            4 -> run0 tracer (pc + 2) prog inputs (outputs ++ [arg 1])
            5 -> run0 tracer (if arg 1 /= 0 then arg 2 else pc + 3) prog inputs outputs
            6 -> run0 tracer (if arg 1 == 0 then arg 2 else pc + 3) prog inputs outputs
            7 -> run0 tracer (pc + 4) (apply (\a b -> if a < b then 1 else 0)) inputs outputs
            8 -> run0 tracer (pc + 4) (apply (\a b -> if a == b then 1 else 0)) inputs outputs
            9 -> run0 tracer (pc + 2) (addRelative prog (arg 1)) inputs outputs
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



disassemble :: Prog -> Addr -> IO ()
disassemble prog offs = do
  let dis = dump prog (fromInteger offs)
  forM_ dis $ \(addr, ints, instr) -> do
    putStrLn ((show addr) ++ "\t" ++ (show instr) ++ "\t\t" ++ (show ints))


runUntil :: Addr -> Tracer Prog
runUntil a prog pc =
  if a /= pc then return $ Nothing
  else do
    disassemble prog 0
    return $ Just prog
