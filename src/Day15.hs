module Day15 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Data.Char

import IntcodeStep

{-
--- Day 15: Oxygen System ---

Out here in deep space, many things can go wrong. Fortunately, many of those things have indicator lights.
 Unfortunately, one of those lights is lit: the oxygen system for part of the ship has failed!

According to the readouts, the oxygen system must have failed days ago after a rupture in oxygen tank two; that
section of the ship was automatically sealed once oxygen levels went dangerously low. A single remotely-operated
repair droid is your only option for fixing the oxygen system.

The Elves' care package included an Intcode program (your puzzle input) that you can use to remotely control the
repair droid. By running that program, you can direct the repair droid to the oxygen system and fix the problem.

The remote control program executes the following steps in a loop forever:

    Accept a movement command via an input instruction.
    Send the movement command to the repair droid.
    Wait for the repair droid to finish the movement operation.
    Report on the status of the repair droid via an output instruction.

Only four movement commands are understood: north (1), south (2), west (3), and east (4). Any other command is
invalid. The movements differ in direction, but not in distance: in a long enough east-west hallway, a series of
commands like 4,4,4,4,3,3,3,3 would leave the repair droid back where it started.

The repair droid can reply with any of the following status codes:

    0: The repair droid hit a wall. Its position has not changed.
    1: The repair droid has moved one step in the requested direction.
    2: The repair droid has moved one step in the requested direction; its new position is the location of the
       oxygen system.

You don't know anything about the area around the repair droid, but you can figure it out by watching the status
codes.

For example, we can draw the area using D for the droid, # for walls, . for locations the droid can traverse, and
empty space for unexplored locations. Then, the initial state looks like this:



   D



To make the droid go north, send it 1. If it replies with 0, you know that location is a wall and that the droid
didn't move:


   #
   D



To move east, send 4; a reply of 1 means the movement was successful:


   #
   .D



Then, perhaps attempts to move north (1), south (2), and east (4) are all met with replies of 0:


   ##
   .D#
    #


Now, you know the repair droid is in a dead end. Backtrack with 3 (which you already know will get a reply of 1
because you already know that location is open):


   ##
   D.#
    #


Then, perhaps west (3) gets a reply of 0, south (2) gets a reply of 1, south again (2) gets a reply of 0, and
then west (3) gets a reply of 2:


   ##
  #..#
  D.#
   #

Now, because of the reply of 2, you know you've found the oxygen system! In this example, it was only 2 moves
away from the repair droid's starting position.

What is the fewest number of movement commands required to move the repair droid from its starting position to
the location of the oxygen system?
-}

type Coord = (Integer, Integer)

data Cell = Wall | Empty | Oxygen | Unknown deriving (Show, Eq)

wall = Wall
empty = Empty
oxygen = Oxygen
unknown = Unknown

type Ship = Map.Map Coord Cell

ship0 = Map.singleton (0, 0) Empty

shipGet ship (x, y) =
  case Map.lookup (x, y) ship of
    Nothing -> Unknown
    Just x -> x

dirToDxDy 1 = (0, 1)
dirToDxDy 2 = (0, -1)
dirToDxDy 3 = (-1, 0)
dirToDxDy 4 = (1, 0)

dxdyToDir (0, 1) = 1
dxdyToDir (0, -1) = 2
dxdyToDir (-1, 0) = 3
dxdyToDir (1, 0) = 4
dxdyToDir dxdy = error $ show dxdy


day15 ls =
  let prog = parse . head $ ls
      ship = ship0
      robot = (0, 0)
      shipZ = runRobotBreadth ship0 (Map.singleton (0,0) prog) :: Ship

  in (pathTo shipZ robot Oxygen & head & length & pred, shipZ)


runRobotBreadth :: Ship -> Map.Map Coord Prog -> Ship
runRobotBreadth ship progMap =
  let path = pathTo ship (0,0) Unknown {- Find the nearest unknown cell -}
  in  if path == [] then ship
      else
        let (target@(x',y'):prev@(x,y):_) = head path & reverse {- nearest target cell, previous coord -}
            prevProg = progMap Map.! prev   {- use the state of the computer at that point -}
            (dx,dy) = (signum $ x' - x, signum $ y' - y)
            (newProg, [result], _) = run prevProg [dxdyToDir (dx,dy)]
            newCell = resultToCell result
            ship' = Map.insert target newCell ship
         in runRobotBreadth ship' (Map.insert target newProg progMap)
 


resultToCell 0 = Wall
resultToCell 1 = Empty
resultToCell 2 = Oxygen


move (x, y) (dx, dy) = (x + dx, y + dy)


{- Search the map along Empty cells from the starting point until we find the thing we're looking for.
   Return the locations along the path that locate the nearest one of those -}
pathTo :: Ship -> Coord -> Cell -> [[Coord]]
pathTo ship coord want =
  let routes = Set.singleton [coord]
      reached = Set.singleton coord
  in  search routes reached
  where
    search :: Set.Set [Coord] -> Set.Set Coord -> [[Coord]]
    search routes reached =
      let (routes', reached') = Set.foldl expand (Set.empty, reached) routes
          foundRoutes = Set.filter (\route -> shipGet ship (head route) == want) routes'
      in  if Set.size foundRoutes /= 0
          then Set.toList foundRoutes & map reverse {- If any cells are the target type, return those routes -}
          else if reached == reached'
          then []
          else search routes' reached'
      where
        test cell = cell == Wall
        expand :: (Set.Set [Coord], Set.Set Coord) -> [Coord] -> (Set.Set [Coord], Set.Set Coord)
        expand (routes, reached) route@(at:path) =
          let n = move at (0, 1)
              cellN = shipGet ship n
              (nRoute, nReached) = if test cellN || Set.member n reached
                                   then (Set.empty, Set.empty)
                                   else (Set.singleton $ n:route, Set.singleton n)
              s = move at (0, -1)
              cellS = shipGet ship s
              (sRoute, sReached) = if test cellS || Set.member s reached
                                   then (Set.empty, Set.empty)
                                   else (Set.singleton $ s:route, Set.singleton s)
              w = move at (-1, 0)
              cellW = shipGet ship w
              (wRoute, wReached) = if test cellW || Set.member w reached
                                   then (Set.empty, Set.empty)
                                   else (Set.singleton $ w:route, Set.singleton w)
              e = move at (1, 0)
              cellE = shipGet ship e
              (eRoute, eReached) = if test cellE || Set.member e reached
                                   then (Set.empty, Set.empty)
                                   else (Set.singleton $ e:route, Set.singleton e)
              routes' = Set.unions [routes, nRoute, sRoute, wRoute, eRoute]
              reached' = Set.unions [reached, nReached, sReached, wReached, eReached]
          in  (routes', reached')

{-
You quickly repair the oxygen system; oxygen gradually fills the area.

Oxygen starts in the location containing the repaired oxygen system. It takes one minute for oxygen to spread to all open locations that are adjacent to a location that already contains oxygen. Diagonal locations are not adjacent.

In the example above, suppose you've used the droid to explore the area fully and have the following map (where locations that currently contain oxygen are marked O):

 ##   
#..## 
#.#..#
#.O.# 
 ###  

Initially, the only location which contains oxygen is the location of the repaired oxygen system. However, after one minute, the oxygen spreads to all open (.) locations that are adjacent to a location containing oxygen:

 ##   
#..## 
#.#..#
#OOO# 
 ###  

After a total of two minutes, the map looks like this:

 ##   
#..## 
#O#O.#
#OOO# 
 ###  

After a total of three minutes:

 ##   
#O.## 
#O#OO#
#OOO# 
 ###  

And finally, the whole region is full of oxygen after a total of four minutes:

 ##   
#OO## 
#O#OO#
#OOO# 
 ###  

So, in this example, all locations contain oxygen after 4 minutes.

Use the repair droid to get a complete map of the area. How many minutes will it take to fill with oxygen?
-}

day15b ls =
  let (_, ship) = day15 ls
      oxygen = Map.filter (== Oxygen) ship & Map.keysSet
  in  (oxygen, flood ship 0 oxygen, printShip ship)
  where
    flood ship t oxygen =
      {- Compute adjacent cells ... -}
      let ns = Set.map (\(x,y) -> Set.fromList [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]) oxygen & Set.unions
      {- ... that don't already have oxygen ... -}
             & (`Set.difference` oxygen)
      {- ... and that are Empty on the map -}
             & Set.filter (\(x,y) -> shipGet ship (x,y) == Empty)
      in  if Set.null ns
          then t
          else flood ship (t + 1) (oxygen `Set.union` ns)


printShip ship =
  let ((x0,y0),(x1,y1)) = shipBounds ship
  in  [shipLine y (x0,x1) | y <- [y0..y1]] & reverse & unlines
  where
    shipBounds ship =
      let xs = ship & Map.keysSet & Set.map fst
          ys = ship & Map.keysSet & Set.map snd
      in  ((Set.findMin xs, Set.findMin ys), (Set.findMax xs, Set.findMax ys))
    shipLine y (x0,x1) = [shipAt (x,y) | x <- [x0..x1]]
    shipAt (0,0) = 'D'
    shipAt (x,y) = shipGet ship (x,y) & shipChar 
    shipChar Unknown = '?'
    shipChar Wall = '#'
    shipChar Empty = '.'
    shipChar Oxygen = 'O'
         