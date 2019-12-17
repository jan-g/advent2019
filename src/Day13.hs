module Day13 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char

import Lib
import IntcodeStep

{-
--- Day 13: Care Package ---

As you ponder the solitude of space and the ever-increasing three-hour roundtrip for messages between you and
 Earth, you notice that the Space Mail Indicator Light is blinking. To help keep you sane, the Elves have sent
 you a care package.

It's a new game for the ship's arcade cabinet! Unfortunately, the arcade is all the way on the other end of the
 ship. Surely, it won't be hard to build your own - the care package even comes with schematics.

The arcade cabinet runs Intcode software like the game the Elves sent (your puzzle input). It has a primitive
 screen capable of drawing square tiles on a grid. The software draws tiles to the screen with output
  instructions: every three output instructions specify the x position (distance from the left), y position
   (distance from the top), and tile id. The tile id is interpreted as follows:

    0 is an empty tile. No game object appears in this tile.
    1 is a wall tile. Walls are indestructible barriers.
    2 is a block tile. Blocks can be broken by the ball.
    3 is a horizontal paddle tile. The paddle is indestructible.
    4 is a ball tile. The ball moves diagonally and bounces off objects.

For example, a sequence of output values like 1,2,3,6,5,4 would draw a horizontal paddle tile (1 tile from the
 left and 2 tiles from the top) and a ball tile (6 tiles from the left and 5 tiles from the top).

Start the game. How many block tiles are on the screen when the game exits?
-}

day13 ls =
  let prog = parse . head $ ls
      output = run prog [] & (\(_, o, _) -> o) & chunksOf 3
      screen = foldl (\m [x, y, t] -> Map.insert (x, y) t m) Map.empty output
  in  Map.foldl (\a t -> if t == 2 then a + 1 else a) 0 screen

{-
The game didn't run because you didn't put in any quarters. Unfortunately, you did not bring any quarters.
 Memory address 0 represents the number of quarters that have been inserted; set it to 2 to play for free.

The arcade cabinet has a joystick that can move left and right. The software reads the position of the joystick
 with input instructions:

    If the joystick is in the neutral position, provide 0.
    If the joystick is tilted to the left, provide -1.
    If the joystick is tilted to the right, provide 1.

The arcade cabinet also has a segment display capable of showing a single number that represents the player's
 current score. When three output instructions specify X=-1, Y=0, the third output instruction is not a tile;
  the value instead specifies the new score to show in the segment display. For example, a sequence of output
   values like -1,0,12345 would show 12345 as the player's current score.

Beat the game by breaking all the blocks. What is your score after the last block is broken?
-}

type Coord = (Integer, Integer)
data Screen = Screen { tiles :: Map.Map Coord Integer
                     , score :: Integer
                     , ball :: Coord
                     , paddle :: Coord
                     }

screen0 = Screen { tiles=Map.empty, score=0, ball=(0,0), paddle=(0,0) }

screenUpdate sc [-1, 0, s] = sc { score=s }
screenUpdate sc@Screen { tiles=t } [x, y, z] = sc { tiles=Map.insert (x, y) z t }

blockCount sc = Map.filter (== 2) (tiles sc) & Map.size

ballPos sc = Map.filter (== 4) (tiles sc) & Map.keys & head
batPos sc = Map.filter (== 3) (tiles sc) & Map.keys & head

doUpdate sc output = foldl screenUpdate sc (chunksOf 3 output)

day13b ls =
  let prog = ls & head & parse
      prog' = set prog 0 2
      sc = screen0
  in  playGame prog' sc []


playGame prog sc input =
  let (prog', output, cont) = run prog input
      sc' = doUpdate sc output
  in  if blockCount sc' == 0 then score sc'
      else if not cont then score sc'
      else
        let ball = ballPos sc'
            bat = batPos sc'
        in  playGame prog' sc' [signum (fst ball - fst bat)]


screenDump sc =
  drawMapWith tilePic (tiles sc) & unlines
  where
    tilePic _ Nothing = '.'
    tilePic _ (Just 0) = '.'
    tilePic _ (Just 1) = 'W'
    tilePic _ (Just 2) = '#'
    tilePic _ (Just 3) = '-'
    tilePic _ (Just 4) = 'o'
