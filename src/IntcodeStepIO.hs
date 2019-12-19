module IntcodeStepIO where

import Intcode (Addr, Prog(..), I(..), A(..), parse, toProg, peek, poke, addRelative, getRelative, dump, opAt, opLen)

import Data.Function ((&))
import Data.Array
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Lib
import Control.Monad.Extra (forM_)

import System.IO

import Text.ParserCombinators.ReadP


newtype Tracer r = Tracer (Prog -> Addr -> [Integer] -> [Integer] -> IO (Either r (Tracer r)))

runIO :: Tracer r -> Prog -> [Integer] -> IO (Either r (Prog, Addr, Outputs, Bool))
runIO tracer prog inputs = run0 tracer 0 prog inputs []

run0 :: Tracer r -> Addr -> Prog -> [Integer] -> [Integer] -> IO (Either r (Prog, Addr, Outputs, Bool))
run0 (Tracer tracer) pc prog inputs outputs = do
  trace <- tracer prog pc inputs outputs
  case trace of
    Left r -> return $ Left r
    Right tracer' ->
      let op = peek prog pc `mod` 100
      in case op of
            99 -> return $ Right (prog, pc, outputs, False)
            1 -> run0 tracer' (pc + 4) (apply (+)) inputs outputs
            2 -> run0 tracer' (pc + 4) (apply (*)) inputs outputs
            3 -> if inputs /= []
                 then run0 tracer' (pc + 2) (poke prog (addr 1) (head inputs)) (tail inputs) outputs
                 else return $ Right (prog, pc, outputs, True)
            4 -> run0 tracer' (pc + 2) prog inputs (outputs ++ [arg 1])
            5 -> run0 tracer' (if arg 1 /= 0 then arg 2 else pc + 3) prog inputs outputs
            6 -> run0 tracer' (if arg 1 == 0 then arg 2 else pc + 3) prog inputs outputs
            7 -> run0 tracer' (pc + 4) (apply (\a b -> if a < b then 1 else 0)) inputs outputs
            8 -> run0 tracer' (pc + 4) (apply (\a b -> if a == b then 1 else 0)) inputs outputs
            9 -> run0 tracer' (pc + 2) (addRelative prog (arg 1)) inputs outputs
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


type Inputs = [Integer]
type Outputs = [Integer]

runUntil :: Addr -> Tracer (Prog, Addr, Inputs, Outputs)
runUntil a =
  let trace prog pc inputs outputs =
        if a /= pc then return $ Right $ runUntil a
        else do
          disassemble prog 0
          return $ Left (prog, pc, inputs, outputs)
  in  Tracer trace

stepFor :: (Num a, Ord a, Enum a) => a -> Tracer (Prog, Addr, Inputs, Outputs)
stepFor n =
  let trace prog pc inputs outputs =
        if n > 0 then return $ Right $ stepFor (pred n)
        else do
          return $ Left (prog, pc, inputs, outputs)
  in  Tracer trace

stepUntil :: Addr -> Tracer (Prog, Addr, Inputs, Outputs)
stepUntil a =
  let trace prog pc inputs outputs =
        if a /= pc then return $ Right $ stepUntil a
        else do
          return $ Left (prog, pc, inputs, outputs)
  in  Tracer trace


debugger :: Prog -> Addr -> [Integer] -> IO ()
debugger prog pc inputs = do
  debugger0 prog pc inputs Set.empty
  
debugger0 :: Prog -> Addr -> [Integer] -> Set.Set Addr -> IO ()
debugger0 prog pc inputs watched = do
  let (_, vals, op) = opAt prog pc
      opStr = (show op)
      opSpaces = max 3 (32 - length opStr)
      valsStr = (show vals)
      vSpaces = max 3 (20 - length valsStr)
      offs = getRelative prog
  putStrLn $ (show pc) ++ "\t" ++
             (show op) ++ (replicate opSpaces ' ') ++ "\t" ++
             valsStr ++ (replicate vSpaces ' ') ++ "\t" ++
             "[@=" ++ (show offs) ++ "]=\t" ++
             ([show (peek prog (x + offs)) ++ "  " | x <- [-5 .. -1]] & concat) ++
             "[" ++ (show $ peek prog offs) ++ "]" ++
             (["  " ++ show (peek prog (x + offs)) | x <- [1..5]] & concat) ++ "\t\t" ++
             (watched & Set.toAscList
                      & map (\a -> "[" ++ (show a) ++ "]=" ++ (show $ peek prog a) ++ "  ")
                      & concat)
  putStr "> "
  hFlush stdout
  line <- getLine

  case parseInputs (
       (string "" <<!! (const ("step", 1))) +++
       (string "s" <<!! (const ("step", 1))) +++
       (string "k" <<!! (const ("skip", 1))) +++
       (char '[' <<!! (const ("[", 0)) <<<< many get) +++
       (string "mem" <<>> skipSpaces >>>> intParser) +++
       (string "st" <<!! (const ("stack", 1))) +++
       (string "w" <<>> skipSpaces >>>> intParser)
       ) line of 
    Just ("step", n) -> step
    Just ("skip", n) -> skip
    Just ("[", _) -> addOutputs line
    Just ("mem", n) -> mem n
    Just ("stack", _) -> stack
    Just ("w", m) -> addWatch m
    otherwise -> do
      putStrLn $ "..? " ++ line

  debugger0 prog pc inputs watched

  where
    showOutputs o = do
      if o /= [] then do
        putStrLn $ "OUTPUT: " ++ (show o)
      else return ()
    
    step = do
      result <- run0 (stepFor 1) pc prog inputs []
      case result of
        Right (prog', pc', outputs, _) -> do
          showOutputs outputs
          debugger prog' pc' inputs
        Left (prog', pc', inputs', outputs) -> do
          showOutputs outputs
          debugger0 prog' pc' inputs' watched

    addOutputs line =
      case parseInputs parseIntegerList line of
        Just inputs' -> do
          putStrLn $ "inputs added: " ++ (show inputs')
          debugger0 prog pc (inputs ++ inputs') watched
        otherwise -> debugger0 prog pc inputs watched

    skip = do
      let (_, _, op) = opAt prog pc
          pc' = pc + opLen op
      result <- run0 (stepUntil pc') pc prog inputs []
      case result of
        Right (prog', pc', outputs, _) -> do
          showOutputs outputs
          debugger0 prog' pc' inputs watched
        Left (prog', pc', inputs', outputs) -> do
          showOutputs outputs
          debugger0 prog' pc' inputs' watched
    
    mem n = do
      putStrLn $ "[" ++ (show n) ++ "] = " ++ (show $ peek prog n)

    stack = do
      let offs = getRelative prog
          mem = [(x, peek prog (offs + x)) | x <- [-5 .. 5]]
      forM_ mem $ \(x, v) -> do
        putStrLn $ "[" ++ (show $ x + offs) ++ "]\t[" ++ (show x) ++ "] = " ++ (show v) 

    addWatch a = debugger0 prog pc inputs (Set.insert a watched)


infixl 8 <<<<
(<<<<) :: ReadP p1 -> ReadP p2 -> ReadP p1
p1 <<<< p2 = do
  x <- p1
  _ <- p2
  return x

infixl 8 >>>>
(>>>>) :: ReadP p1 -> ReadP p2 -> ReadP p2
p1 >>>> p2 = do
  _ <- p1
  p2

infixl 6 <<>>
(<<>>) :: ReadP p1 -> ReadP p2 -> ReadP (p1, p2)
p1 <<>> p2 = do
  x <- p1
  y <- p2
  return (x, y)

infixl 8 <<!!
(<<!!) :: ReadP p1 -> (p1 -> p2) -> ReadP p2
p1 <<!! f = do
  x <- p1
  return $ f x

  
parseInputs parser s =
  case readP_to_S (parser <<<< eof) s of
    [(e, "")] -> Just e
    otherwise -> Nothing

parseIntegerList = (char '[') >>>> sepBy intParser (char ',') <<<< (char ']')
