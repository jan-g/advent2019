module Day24 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char

import Lib

{-
--- Day 24: Planet of Discord ---

You land on Eris, your last stop before reaching Santa. As soon as you do, your sensors start picking up strange
life forms moving around: Eris is infested with bugs! With an over 24-hour roundtrip for messages between you
and Earth, you'll have to deal with this problem on your own.

Eris isn't a very large place; a scan of the entire area fits into a 5x5 grid (your puzzle input). The scan shows
bugs (#) and empty spaces (.).

Each minute, The bugs live and die based on the number of bugs in the four adjacent tiles:

    A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
    An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.

Otherwise, a bug or empty space remains the same. (Tiles on the edges of the grid have fewer than four adjacent
tiles; the missing tiles count as empty space.) This process happens in every location simultaneously; that is,
within the same minute, the number of adjacent bugs is counted for every tile first, and then the tiles are updated.

Here are the first few minutes of an example scenario:

Initial state:
    ....#
    #..#.
    #..##
    ..#..
    #....

After 1 minute:
    #..#.
    ####.
    ###.#
    ##.##
    .##..

After 2 minutes:
    #####
    ....#
    ....#
    ...#.
    #.###

After 3 minutes:
    #....
    ####.
    ...##
    #.##.
    .##.#

After 4 minutes:
    ####.
    ....#
    ##..#
    .....
    ##...

To understand the nature of the bugs, watch for the first time a layout of bugs and empty spaces matches any
previous layout. In the example above, the first layout to appear twice is:

    .....
    .....
    .....
    #....
    .#...

To calculate the biodiversity rating for this layout, consider each tile left-to-right in the top row, then
left-to-right in the second row, and so on. Each of these tiles is worth biodiversity points equal to increasing
powers of two: 1, 2, 4, 8, 16, 32, and so on. Add up the biodiversity points for tiles with bugs; in this
example, the 16th tile (32768 points) and 22nd tile (2097152 points) have bugs, a total biodiversity rating of
2129920.

What is the biodiversity rating for the first layout that appears twice?
-}

type Grid = Map.Map (Integer, Integer) Int

parse :: [String] -> Grid
parse ls = [((x, y), b) | (y, line) <- [0..] `zip` ls,
                          (x, c) <- [0..] `zip` line,
                          let b = if c == '#' then 1 else 0]
         & Map.fromList

at grid (x, y) = case Map.lookup (x, y) grid of Nothing -> 0; Just b -> b

draw grid = unlines $ drawMapWith (\_ (Just b) -> if b == 1 then '#' else '.') grid

next grid = Map.mapWithKey (\(x, y) b ->
                             let n = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
                                   & map (at grid)
                                   & sum
                             in case b of
                                  0 -> if n == 1 || n == 2 then 1 else 0
                                  1 -> if n == 1 then 1 else 0) grid

biodiversity grid = Map.foldlWithKey (\s (x, y) b -> s + b * 2 ^ (y * 5 + x)) 0 grid

day24 ls =
  let grid0 = parse ls
      result = loop grid0 (Set.empty)
  in  (draw grid0, result)
  where
    loop g seen =
      let b = biodiversity g
          g' = next g
      in  if Set.member b seen then (g, b)
          else loop g' (Set.insert b seen)

{-
--- Part Two ---

After careful analysis, one thing is certain: you have no idea where all these bugs are coming from.

Then, you remember: Eris is an old Plutonian settlement! Clearly, the bugs are coming from recursively-folded
space.

This 5x5 grid is only one level in an infinite number of recursion levels. The tile in the middle of the grid
is actually another 5x5 grid, the grid in your scan is contained as the middle tile of a larger 5x5 grid, and
so on. Two levels of grids look like this:

         |     |         |     |
         |     |         |     |
         |     |         |     |
    -----+-----+---------+-----+-----
         |     |         |     |
         |     |         |     |
         |     |         |     |
    -----+-----+---------+-----+-----
         |     | | | | | |     |
         |     |-+-+-+-+-|     |
         |     | | | | | |     |
         |     |-+-+-+-+-|     |
         |     | | |?| | |     |
         |     |-+-+-+-+-|     |
         |     | | | | | |     |
         |     |-+-+-+-+-|     |
         |     | | | | | |     |
    -----+-----+---------+-----+-----
         |     |         |     |
         |     |         |     |
         |     |         |     |
    -----+-----+---------+-----+-----
         |     |         |     |
         |     |         |     |
         |     |         |     |

(To save space, some of the tiles are not drawn to scale.) Remember, this is only a small part of the infinitely
recursive grid; there is a 5x5 grid that contains this diagram, and a 5x5 grid that contains that one, and so
on. Also, the ? in the diagram contains another 5x5 grid, which itself contains another 5x5 grid, and so on.

The scan you took (your puzzle input) shows where the bugs are on a single level of this structure. The middle
tile of your scan is empty to accommodate the recursive grids within it. Initially, no other levels contain bugs.

Tiles still count as adjacent if they are directly up, down, left, or right of a given tile. Some tiles have
adjacent tiles at a recursion level above or below its own level. For example:

         |     |         |     |
      1  |  2  |    3    |  4  |  5
         |     |         |     |
    -----+-----+---------+-----+-----
         |     |         |     |
      6  |  7  |    8    |  9  |  10
         |     |         |     |
    -----+-----+---------+-----+-----
         |     |A|B|C|D|E|     |
         |     |-+-+-+-+-|     |
         |     |F|G|H|I|J|     |
         |     |-+-+-+-+-|     |
     11  | 12  |K|L|?|N|O|  14 |  15
         |     |-+-+-+-+-|     |
         |     |P|Q|R|S|T|     |
         |     |-+-+-+-+-|     |
         |     |U|V|W|X|Y|     |
    -----+-----+---------+-----+-----
         |     |         |     |
     16  | 17  |    18   |  19 |  20
         |     |         |     |
    -----+-----+---------+-----+-----
         |     |         |     |
     21  | 22  |    23   |  24 |  25
         |     |         |     |

    Tile 19 has four adjacent tiles: 14, 18, 20, and 24.
    Tile G has four adjacent tiles: B, F, H, and L.
    Tile D has four adjacent tiles: 8, C, E, and I.
    Tile E has four adjacent tiles: 8, D, 14, and J.
    Tile 14 has eight adjacent tiles: 9, E, J, O, T, Y, 15, and 19.
    Tile N has eight adjacent tiles: I, O, S, and five tiles within the sub-grid marked ?.

The rules about bugs living and dying are the same as before.

For example, consider the same initial state as above:

    ....#
    #..#.
    #.?##
    ..#..
    #....

The center tile is drawn as ? to indicate the next recursive grid. Call this level 0; the grid within this one is level 1, and the grid that contains this one is level -1. Then, after ten minutes, the grid at each level would look like this:

Depth -5:
    ..#..
    .#.#.
    ..?.#
    .#.#.
    ..#..

Depth -4:
    ...#.
    ...##
    ..?..
    ...##
    ...#.

Depth -3:
    #.#..
    .#...
    ..?..
    .#...
    #.#..

Depth -2:
    .#.##
    ....#
    ..?.#
    ...##
    .###.

Depth -1:
    #..##
    ...##
    ..?..
    ...#.
    .####

Depth 0:
    .#...
    .#.##
    .#?..
    .....
    .....

Depth 1:
    .##..
    #..##
    ..?.#
    ##.##
    #####

Depth 2:
    ###..
    ##.#.
    #.?..
    .#.##
    #.#..

Depth 3:
    ..###
    .....
    #.?..
    #....
    #...#

Depth 4:
    .###.
    #..#.
    #.?..
    ##.#.
    .....

Depth 5:
    ####.
    #..#.
    #.?#.
    ####.
    .....

In this example, after 10 minutes, a total of 99 bugs are present.

Starting with your scan, how many bugs are present after 200 minutes?
-}

type Grid' = Map.Map (Int, Int, Int) Int

adj (2, 2, _) = []

adj (0, 0, z) = [(1, 0, z), (0, 1, z), (2, 1, z-1), (1, 2, z-1)]
adj (1, 0, z) = [(0, 0, z), (2, 0, z), (1, 1, z), (2, 1, z-1)]
adj (2, 0, z) = [(1, 0, z), (3, 0, z), (2, 1, z), (2, 1, z-1)]
adj (3, 0, z) = [(2, 0, z), (4, 0, z), (3, 1, z), (2, 1, z-1)]
adj (4, 0, z) = [(3, 0, z), (3, 2, z-1), (4, 1, z), (2, 1, z-1)]

adj (0, 1, z) = [(1, 2, z-1), (1, 1, z), (0, 0, z), (0, 2, z)]
--adj (1, 1, z) =
adj (2, 1, z) = [(1, 1, z), (3, 1, z), (2, 0, z), (0, 0, z+1), (1, 0, z+1), (2, 0, z+1), (3, 0, z+1), (4, 0, z+1)]
--adj (3, 1, z) =
adj (4, 1, z) = [(3, 1, z), (3, 2, z-1), (4, 0, z), (4, 2, z)]

adj (0, 2, z) = [(1, 2, z-1), (1, 2, z), (0, 1, z), (0, 3, z)]
adj (1, 2, z) = [(0, 2, z), (0, 1, z), (0, 3, z), (0, 0, z+1), (0, 1, z+1), (0, 2, z+1), (0, 3, z+1), (0, 4, z+1)]
--adj (2, 2, z) =
adj (3, 2, z) = [(4, 2, z), (3, 1, z), (3, 3, z), (4, 0, z+1), (4, 1, z+1), (4, 2, z+1), (4, 3, z+1), (4, 4, z+1)]
adj (4, 2, z) = [(3, 1, z), (3, 2, z-1), (4, 1, z), (4, 3, z)]

adj (0, 3, z) = [(1, 2, z-1), (1, 3, z), (0, 2, z), (0, 4, z)]
--adj (1, 3, z) =
adj (2, 3, z) = [(1, 3, z), (3, 3, z), (2, 4, z), (0, 4, z+1), (1, 4, z+1), (2, 4, z+1), (3, 4, z+1), (4, 4, z+1)]
--adj (3, 3, z) =
adj (4, 3, z) = [(3, 3, z), (3, 2, z-1), (4, 2, z), (4, 4, z)]

adj (0, 4, z) = [(1, 2, z-1), (1, 4, z), (0, 3, z), (2, 3, z-1)]
adj (1, 4, z) = [(0, 4, z), (2, 4, z), (1, 3, z), (2, 3, z-1)]
adj (2, 4, z) = [(1, 4, z), (3, 4, z), (2, 3, z), (2, 3, z-1)]
adj (3, 4, z) = [(2, 4, z), (4, 4, z), (3, 3, z), (2, 3, z-1)]
adj (4, 4, z) = [(3, 4, z), (3, 2, z-1), (4, 3, z), (2, 3, z-1)]

adj (x, y, z) = [(x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z)]


adj' (2, 2, _) = []
adj' (x, y, z) = (if x > 0 then [(x-1, y, z)]
                           else [(1, 2, z-1)]) ++
                 (if x < 4 then [(x+1, y, z)]
                           else [(3, 2, z-1)]) ++
                 (if y > 0 then [(x, y-1, z)]
                           else [(2, 1, z-1)]) ++
                 (if y < 4 then [(x, y+1, z)]
                           else [(2, 3, z-1)]) ++
                 (if (x, y) == (2, 1)
                           then [(x', 0, z+1) | x' <- [0..4]]
                           else []) ++
                 (if (x, y) == (2, 3)
                           then [(x', 4, z+1) | x' <- [0..4]]
                           else []) ++
                 (if (x, y) == (1, 2)
                           then [(0, y', z+1) | y' <- [0..4]]
                           else []) ++
                 (if (x, y) == (3, 2)
                           then [(4, y', z+1) | y' <- [0..4]]
                           else [])
                          

at' grid (x, y, z) = case Map.lookup (x, y, z) grid of
                       Nothing -> 0
                       Just b -> b

bounds' grid =
  let z0 = Map.keysSet grid & Set.map (\(x, y, z) -> z) & minimum & pred
      z1 = Map.keysSet grid & Set.map (\(x, y, z) -> z) & maximum & succ
  in (z0, z1)

next' grid =
  let (z0, z1) = bounds' grid
  in  [((x, y, z), b) | x <- [0..4], y <- [0..4], z <- [z0..z1],
                        let n = adj' (x, y, z) & map (at' grid) & sum,
                        let b = case at' grid (x, y, z) of
                                  0 -> if n == 1 || n == 2 then 1 else 0
                                  1 -> if n == 1 then 1 else 0]
       & Map.fromList

parse' ls = parse ls
          & Map.mapKeysWith const (\(x, y) -> (x, y, 0))


draw' grid =
  let (z0,z1) = bounds' grid
  in  unlines [draw'' z | z <- [succ z0..pred z1]]
  where
    draw'' z =
      let output = grid
                 & Map.filterWithKey (\(_, _, z') _ -> z == z')
                 & Map.mapKeys (\(x, y, z) -> (x, y))
                 & draw
      in  "Level " ++ show z ++ "\n" ++ output ++ "\n"


day24b ls =
  let grid0 = parse' ls
      gridN = iterate next' grid0 !! 200
   in Map.foldl (+) 0 gridN
