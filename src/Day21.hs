module Day21 where

import Control.Monad (when)

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char

import Lib
import IntcodeStepIO
import qualified Intcode as I
import System.IO

{-
--- Day 21: Springdroid Adventure ---

You lift off from Pluto and start flying in the direction of Santa.

While experimenting further with the tractor beam, you accidentally pull an asteroid directly into your ship! It deals significant damage to your hull and causes your ship to begin tumbling violently.

You can send a droid out to investigate, but the tumbling is causing enough artificial gravity that one wrong step could send the droid through a hole in the hull and flying out into space.

The clear choice for this mission is a droid that can jump over the holes in the hull - a springdroid.

You can use an Intcode program (your puzzle input) running on an ASCII-capable computer to program the springdroid. However, springdroids don't run Intcode; instead, they run a simplified assembly language called springscript.

While a springdroid is certainly capable of navigating the artificial gravity and giant holes, it has one downside: it can only remember at most 15 springscript instructions.

The springdroid will move forward automatically, constantly thinking about whether to jump. The springscript program defines the logic for this decision.

Springscript programs only use Boolean values, not numbers or strings. Two registers are available: T, the temporary value register, and J, the jump register. If the jump register is true at the end of the springscript program, the springdroid will try to jump. Both of these registers start with the value false.

Springdroids have a sensor that can detect whether there is ground at various distances in the direction it is facing; these values are provided in read-only registers. Your springdroid can detect ground at four distances: one tile away (A), two tiles away (B), three tiles away (C), and four tiles away (D). If there is ground at the given distance, the register will be true; if there is a hole, the register will be false.

There are only three instructions available in springscript:

    AND X Y sets Y to true if both X and Y are true; otherwise, it sets Y to false.
    OR X Y sets Y to true if at least one of X or Y is true; otherwise, it sets Y to false.
    NOT X Y sets Y to true if X is false; otherwise, it sets Y to false.

In all three instructions, the second argument (Y) needs to be a writable register (either T or J). The first argument (X) can be any register (including A, B, C, or D).

For example, the one-instruction program NOT A J means "if the tile immediately in front of me is not ground, jump".

Or, here is a program that jumps if a three-tile-wide hole (with ground on the other side of the hole) is detected:

NOT A J
NOT B T
AND T J
NOT C T
AND T J
AND D J

The Intcode program expects ASCII inputs and outputs. It will begin by displaying a prompt; then, input the desired instructions one per line. End each line with a newline (ASCII code 10). When you have finished entering your program, provide the command WALK followed by a newline to instruct the springdroid to begin surveying the hull.

If the springdroid falls into space, an ASCII rendering of the last moments of its life will be produced. In these, @ is the springdroid, # is hull, and . is empty space. For example, suppose you program the springdroid like this:

NOT D J
WALK

This one-instruction program sets J to true if and only if there is no ground four tiles away. In other words, it attempts to jump into any hole it finds:

    .................
    .................
    @................
    #####.###########

    .................
    .................
    .@...............
    #####.###########

    .................
    ..@..............
    .................
    #####.###########

    ...@.............
    .................
    .................
    #####.###########

    .................
    ....@............
    .................
    #####.###########

    .................
    .................
    .....@...........
    #####.###########

    .................
    .................
    .................
    #####@###########

However, if the springdroid successfully makes it across, it will use an output instruction to indicate the amount of damage to the hull as a single giant integer outside the normal ASCII range.

Program the springdroid with logic that allows it to survey the hull without falling into space. What amount of hull damage does it report?

-}

day21' ls = do
  let prog = parse $ head ls
  let input0 = "OR A T\n"  ++
               "AND C T\n" ++  -- T = false if there's a hole in any of 1, 2, 3 steps
               "NOT T J\n" ++  -- jump over those, but
               "AND D J\n" ++  -- only if there's somewhere to land
               "WALK\n"
             & toAscii
  loop prog 0 input0
  putStrLn "Program ended"

loop prog pc input = do
  {- run the program for a bit -}
  Right (prog', pc', output, continue) <- run0 nullTracer pc prog input []
  writeOutput output
  if continue then do
    line <- getLine
    let line' = line ++ "\n"
    loop prog' pc' (line' & map ord & map toInteger)
  else
    return ()

writeOutput o = do
  putStr $ fromAscii o
  hFlush stdout

fromAscii o = o & map (\c -> if c < 256 then [c & fromInteger & chr] else "[" ++ show c ++ "]") & concat

toAscii i = i & map ord & map toInteger

{-
There are many areas the springdroid can't reach. You flip through the manual and discover a way to increase its sensor range.

Instead of ending your springcode program with WALK, use RUN. Doing this will enable extended sensor mode, capable of sensing ground up to nine tiles away. This data is available in five new read-only registers:

    Register E indicates whether there is ground five tiles away.
    Register F indicates whether there is ground six tiles away.
    Register G indicates whether there is ground seven tiles away.
    Register H indicates whether there is ground eight tiles away.
    Register I indicates whether there is ground nine tiles away.

All other functions remain the same.

Successfully survey the rest of the hull by ending your program with RUN. What amount of hull damage does the springdroid now report?
-}

day21b ls = do
  let prog = parse $ head ls
  {- if (^a | ^b | ^c) & d & (h | e & (i | f)) => jump -}
  {- d && (h | e & i | e & f) => jump -}
  let input0 = "OR E J\n"  ++
               "AND E J\n" ++  -- J = E
               "AND I J\n" ++  -- J = E & I
               "OR  H J\n" ++  -- J = H | E & I
               "OR E T\n" ++
               "AND E T\n" ++
               "AND F T\n" ++
               "OR T J\n" ++

               "AND D J\n" ++  -- J = false if there's a hole in any of 1, 2, 3 steps

               "OR  A T\n" ++
               "AND A T\n" ++  -- T = A
               "AND B T\n" ++
               "AND C T\n" ++  -- T = True if it's okay to walk to D
               "NOT T T\n" ++
               "AND T J\n" ++  -- just walk for a bit
               "RUN\n"
             & toAscii
  loop prog 0 input0
  putStrLn "Program ended"


day21 ls = do
  putStrLn $ show $ length instr
  let prog = parse $ head ls
  (trial, ans) <- try prog (drop 47987 inputs & filter (not . redundant)) 0
  putStrLn $ "trial:\n" ++ trial ++ "\n" ++ show ans
  where
    try prog (p:ps) n = do
      let t = toStr p
      Control.Monad.when (n `mod` 10 == 0) $ do
        putStrLn $ show n ++ " " ++ show t
        hFlush stdout
      let (_, out) = I.run prog (toAscii t)
          result = dropWhile (< 256) out & take 1
      if length result == 1
        then return (t, result)
        else try prog ps (succ n)

    instr :: [I]
    instr = do
      from <- [D, C, B, A, T, J]
      to <- [T, J]
      instr <- [And, Or, Not]
      return $ instr from to

    inputs :: [[I]]
    inputs = [[]] ++ [i : s | s <- inputs, i <- instr]
    
    redundant :: [I] -> Bool
    redundant [] = False
    redundant [op] = dest op /= J
    redundant [o1, o2]
      | o1 == o2 = True
      | dest o1 == T = src o2 /= T
      | otherwise = redundant [o2]
    redundant (a:b:cs)
      | a == b = True
      | dest a == dest b && isNot b && src b /= dest b = True
      | otherwise = redundant (b:cs)
    
    toStr :: [I] -> String
    toStr i = (map show i) & concat & (++ "WALK\n")

dest (Not _ d) = d
dest (And _ d) = d
dest (Or _ d) = d
src (Not s _) = s
src (And s _) = s
src (Or s _) = s
isNot (Not _ _) = True
isNot _ = False

data I = Not S S | And S S | Or S S deriving (Eq)
instance Show I where
  show (Not x y) = "NOT " ++ show x ++ " " ++ show y ++ "\n"
  show (And x y) = "AND " ++ show x ++ " " ++ show y ++ "\n"
  show (Or x y) = "OR " ++ show x ++ " " ++ show y ++ "\n"

data S = A | B | C | D | T | J deriving (Eq, Show)
