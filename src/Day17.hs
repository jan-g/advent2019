module Day17 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.List.Extra (inits, minimumOn)

import Intcode

{-
--- Day 17: Set and Forget ---

An early warning system detects an incoming solar flare and automatically activates the ship's electromagnetic
shield. Unfortunately, this has cut off the Wi-Fi for many small robots that, unaware of the impending danger,
are now trapped on exterior scaffolding on the unsafe side of the shield. To rescue them, you'll have to act
quickly!

The only tools at your disposal are some wired cameras and a small vacuum robot currently asleep at its charging
station. The video quality is poor, but the vacuum robot has a needlessly bright LED that makes it easy to spot
no matter where it is.

An Intcode program, the Aft Scaffolding Control and Information Interface (ASCII, your puzzle input), provides
access to the cameras and the vacuum robot. Currently, because the vacuum robot is asleep, you can only access
the cameras.

Running the ASCII program on your Intcode computer will provide the current view of the scaffolds. This is output,
purely coincidentally, as ASCII code: 35 means #, 46 means ., 10 starts a new line of output below the current one,
and so on. (Within a line, characters are drawn left-to-right.)

In the camera output, # represents a scaffold and . represents open space. The vacuum robot is visible as
^, v, <, or > depending on whether it is facing up, down, left, or right respectively. When drawn like this, the
vacuum robot is always on a scaffold; if the vacuum robot ever walks off of a scaffold and begins tumbling through
space uncontrollably, it will instead be visible as X.

In general, the scaffold forms a path, but it sometimes loops back onto itself. For example, suppose you can see
the following view from the cameras:

..#..........
..#..........
#######...###
#.#...#...#.#
#############
..#...#...#..
..#####...^..

Here, the vacuum robot, ^ is facing up and sitting at one end of the scaffold near the bottom-right of the image.
The scaffold continues up, loops across itself several times, and ends at the top-left of the image.

The first step is to calibrate the cameras by getting the alignment parameters of some well-defined points. Locate
all scaffold intersections; for each, its alignment parameter is the distance between its left edge and the left
edge of the view multiplied by the distance between its top edge and the top edge of the view. Here, the
intersections from the above image are marked O:

..#..........
..#..........
##O####...###
#.#...#...#.#
##O###O###O##
..#...#...#..
..#####...^..

For these intersections:

    The top-left intersection is 2 units from the left of the image and 2 units from the top of the image, so its
     alignment parameter is 2 * 2 = 4.
    The bottom-left intersection is 2 units from the left and 4 units from the top, so its alignment parameter
     is 2 * 4 = 8.
    The bottom-middle intersection is 6 from the left and 4 from the top, so its alignment parameter is 24.
    The bottom-right intersection's alignment parameter is 40.

To calibrate the cameras, you need the sum of the alignment parameters. In the above example, this is 76.

Run your ASCII program. What is the sum of the alignment parameters for the scaffold intersections?
-}

type Coord = (Integer, Integer)
type Scaffold = Map.Map Coord Char

isScaffold :: Char -> Bool
isScaffold '^' = True
isScaffold 'v' = True
isScaffold '<' = True
isScaffold '>' = True
isScaffold '#' = True
isScaffold _ = False

getScaf :: Scaffold -> Coord -> Char
getScaf scaf (x, y) = case Map.lookup (x, y) scaf of Nothing -> '.'; Just c -> c

textToScaf lines = [((x, y), c) | (y, line) <- [0..] `zip` lines, (x, c) <- [0..] `zip` line] & Map.fromList

intersections scaf = Map.filterWithKey (\(x, y) c -> [(x, y), (x-1, y), (x+1, y), (x, y-1), (x, y+1)]
                                                   & map (getScaf scaf)
                                                   & all isScaffold) scaf
                   & Map.keysSet

day17 ls =
  let prog = parse $ head ls
      (_, output) = run prog []
      text = map (chr . fromInteger) output & lines
      (x0, y0) = (0, 0)
      x1 = text & head & length & pred
      y1 = text & length & pred
      scaf = textToScaf text
      ints = intersections scaf
  in ints & Set.toList & map (\(x, y) -> x * y) & sum

{-
Now for the tricky part: notifying all the other robots about the solar flare. The vacuum robot can do this
automatically if it gets into range of a robot. However, you can't see the other robots on the camera, so you
need to be thorough instead: you need to make the vacuum robot visit every part of the scaffold at least once.

The vacuum robot normally wanders randomly, but there isn't time for that today. Instead, you can override its
movement logic with new rules.

Force the vacuum robot to wake up by changing the value in your ASCII program at address 0 from 1 to 2. When you
do this, you will be automatically prompted for the new movement rules that the vacuum robot should use. The
ASCII program will use input instructions to receive them, but they need to be provided as ASCII code; end each
line of logic with a single newline, ASCII code 10.

First, you will be prompted for the main movement routine. The main routine may only call the movement functions:
A, B, or C. Supply the movement functions to use as ASCII text, separating them with commas (,, ASCII code 44),
and ending the list with a newline (ASCII code 10). For example, to call A twice, then alternate between B and C
three times, provide the string A,A,B,C,B,C,B,C and then a newline.

Then, you will be prompted for each movement function. Movement functions may use L to turn left, R to turn
right, or a number to move forward that many units. Movement functions may not call other movement functions.
Again, separate the actions with commas and end the list with a newline. For example, to move forward 10 units,
turn left, move forward 8 units, turn right, and finally move forward 6 units, provide the string 10,L,8,R,6 and
then a newline.

Finally, you will be asked whether you want to see a continuous video feed; provide either y or n and a newline.
Enabling the continuous video feed can help you see what's going on, but it also requires a significant amount of
processing power, and may even cause your Intcode computer to overheat.

Due to the limited amount of memory in the vacuum robot, the ASCII definitions of the main routine and the
movement functions may each contain at most 20 characters, not counting the newline.

For example, consider the following camera feed:

#######...#####
#.....#...#...#
#.....#...#...#
......#...#...#
......#...###.#
......#.....#.#
^########...#.#
......#.#...#.#
......#########
........#...#..
....#########..
....#...#......
....#...#......
....#...#......
....#####......

In order for the vacuum robot to visit every part of the scaffold at least once, one path it could take is:

R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2

Without the memory limit, you could just supply this whole string to function A and have the main routine call
A once. However, you'll need to split it into smaller parts.

One approach is:

    Main routine: A,B,C,B,A,C
    (ASCII input: 65, 44, 66, 44, 67, 44, 66, 44, 65, 44, 67, 10)
    Function A:   R,8,R,8
    (ASCII input: 82, 44, 56, 44, 82, 44, 56, 10)
    Function B:   R,4,R,4,R,8
    (ASCII input: 82, 44, 52, 44, 82, 44, 52, 44, 82, 44, 56, 10)
    Function C:   L,6,L,2
    (ASCII input: 76, 44, 54, 44, 76, 44, 50, 10)

Visually, this would break the desired path into the following parts:

A,        B,            C,        B,            A,        C
R,8,R,8,  R,4,R,4,R,8,  L,6,L,2,  R,4,R,4,R,8,  R,8,R,8,  L,6,L,2

CCCCCCA...BBBBB
C.....A...B...B
C.....A...B...B
......A...B...B
......A...CCC.B
......A.....C.B
^AAAAAAAA...C.B
......A.A...C.B
......AAAAAA#AB
........A...C..
....BBBB#BBBB..
....B...A......
....B...A......
....B...A......
....BBBBA......

Of course, the scaffolding outside your ship is much more complex.

As the vacuum robot finds other robots and notifies them of the impending solar flare, it also can't help but
leave them squeaky clean, collecting any space dust it finds. Once it finishes the programmed set of movements,
assuming it hasn't drifted off into space, the cleaning robot will return to its docking station and report the
amount of space dust it collected as a large, non-ASCII value in a single output instruction.

After visiting every part of the scaffold at least once, how much dust does the vacuum robot report it has
collected?
-}

data Robot = Robot Coord Coord
type Trail = Set.Set Coord

{- +ve x is right; +ve y is down -}
right (Robot xy (dx, dy)) = Robot xy (-dy, dx)
left (Robot xy (dx, dy)) = Robot xy (dy, -dx)
forward (Robot (x, y) (dx, dy)) = Robot (x + dx, y + dy) (dx, dy)

step :: Robot -> Char -> Robot
step r 'L' = left r
step r 'R' = right r
step r '1' = forward r

locateRobot scaf =
  let (x, y) = scaf & Map.filter (== '^') & Map.keysSet & Set.findMin
  in  Robot (x, y) (0, -1)

xy (Robot c _) = c

day17b ls =
  let prog = parse $ head ls
      (_, output) = run prog []
      text = map (chr . fromInteger) output & lines
      (x0, y0) = (0, 0)
      x1 = text & head & length & pred
      y1 = text & length & pred
      scaf = textToScaf text
      robot = locateRobot scaf
      route = findRoute scaf robot (Set.singleton $ xy robot)
      trace = scanl step robot route & map xy & Set.fromList
      tracedAll = trace `Set.difference` Map.keysSet scaf
      {- (a, b, c) = ("R6L12R6","L12R6L8L12","R12L10L10") -}
      {- The answer is from the following - but it took about 30s to compute -}
      (a, b, c, route') = shorten route

      {- Force the vacuum robot to wake up by changing the value in your ASCII program at address 0 from 1 to 2. -}
      prog' = poke prog 0 2
      {- First, you will be prompted for the main movement routine. Supply the movement functions to use as ASCII
         text, separating them with commas (,, ASCII code 44),
         and ending the list with a newline (ASCII code 10). -}
      input = route' ++ "\n"
      {- Then, you will be prompted for each movement function. -}
              ++ a ++ "\n"
              ++ b ++ "\n"
              ++ c ++ "\n"
      {- Finally, you will be asked whether you want to see a continuous video feed; provide either y or n and a
         newline. -}
              ++ "n\n"

      (_, output') = run prog' (map (toInteger . ord) input)
      output'' = take (length output' & pred) output'  :: [Integer]
      answer = drop (length output' & pred) output' & head  :: Integer
  in (a, b, c, route',
      map (chr . fromInteger) output''  :: String,
      answer :: Integer)


at scaf (Robot xy _) = getScaf scaf xy & isScaffold
seen visited (Robot xy _) = Set.member xy visited

findRoute :: Scaffold -> Robot -> Set.Set Coord -> String
findRoute scaf robot visited =
  {- If there's nowhere left to go, stop -}
  {- The positions near us -}
  let (l, f, r) = (left robot & forward, forward robot, right robot & forward)
      (canL, canF, canR) = (at scaf l, at scaf f, at scaf r)
  in  if canF then "1" ++ (findRoute scaf f (Set.insert (xy f) visited))
      else if not canL && not canR then ""
      else if canL && (not $ seen visited l) then "L1" ++ (findRoute scaf l (Set.insert (xy l) visited))
      else if canR && (not $ seen visited r) then "R1" ++ (findRoute scaf r (Set.insert (xy r) visited))
      else "?"

replaceRoute r a b c = replaceRoute0 r (uncompress a) (uncompress b) (uncompress c)

replaceRoute0 "" _ _ _ = ""
replaceRoute0 route a b c =
  if      take (length a) route == a then "A" ++ (replaceRoute0 (drop (length a) route) a b c)
  else if take (length b) route == b then "B" ++ (replaceRoute0 (drop (length b) route) a b c)
  else if take (length c) route == c then "C" ++ (replaceRoute0 (drop (length c) route) a b c)
  else    (take 1 route) ++ (replaceRoute0 (tail route) a b c)


shorten route =
  {- Find an A that has maximal effect -}
  let abcr = [(a', b', c', r''') |
                       {- Candidates for A come at the start of the sequence -}
                       a <- tail (inits route),
                       let a' = rewrite $ compress a,
                       length a' <= 20,
                       let r' = replaceRoute0 route a "X" "X",
                       {- Candidates for B might as well immediately follow the prefix of As -}
                       b <- dropWhile (=='A') r' & takeWhile (/='A') & inits & tail,
                       let b' = rewrite $ compress b,
                       length b' <= 20,
                       let r'' = replaceRoute0 route a b "X",
                       {- And similarly for candidates for C -}
                       c <- dropWhile (\x -> x=='A' || x=='B') r''
                          & takeWhile (\x -> x/='A' && x/='B')
                          & inits & tail,
                       let c' = rewrite $ compress c,
                       length c' <= 20,
                       let r''' = rewrite $ replaceRoute0 route a b c,
                       {- After extracting As, Bs and Cs, there must be nothing left -}
                       filter (\c -> c /= 'A' && c /= 'B' && c /= 'C' && c /= ',') r''' == "",
                       length r''' <= 20]
      (a, b, c, r) = minimumOn (\(a, b, c, r) -> (length $ a ++ b ++ c) + (2 * length r)) abcr
  in (a, b, c, r)


compress :: String -> String
compress "" = ""
compress r@('1':rs) =
  let fs = takeWhile (== '1') r & length
  in  (show fs) ++ compress (drop fs r)
compress (c:rs) = c:(compress rs)

uncompress "" = ""
uncompress r@(d:rs)
  | isDigit d = let n = takeWhile isDigit r
                    fs = read n
                    in  (replicate fs '1') ++ (uncompress $ drop (length n) r)
uncompress (x:rs) = x : (uncompress rs)

rewrite "" = ""
rewrite x = rewrite0 x & tail
rewrite0 "" = ""
rewrite0 r@(d:rs)
  | isDigit d = let n = takeWhile isDigit r
                in  "," ++ n ++ rewrite0 (drop (length n) r)
rewrite0 (x:rs) = "," ++ x : (rewrite0 rs)

unrewrite p = p & splitOn "," & map (\c -> case c of
                                        "R" -> "R"
                                        "L" -> "L"
                                        d -> replicate (read d) '1') & concat
