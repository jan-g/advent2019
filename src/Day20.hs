module Day20 where

import Data.Function ((&))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char
import Data.Maybe (isJust, fromJust)

import Lib

{-
--- Day 20: Donut Maze ---

You notice a strange pattern on the surface of Pluto and land nearby to get a closer look. Upon closer inspection, you realize you've come across one of the famous space-warping mazes of the long-lost Pluto civilization!

Because there isn't much space on Pluto, the civilization that used to live here thrived by inventing a method for folding spacetime. Although the technology is no longer understood, mazes like this one provide a small glimpse into the daily life of an ancient Pluto citizen.

This maze is shaped like a donut. Portals along the inner and outer edge of the donut can instantly teleport you from one side to the other. For example:

             A           
             A           
      #######.#########  
      #######.........#  
      #######.#######.#  
      #######.#######.#  
      #######.#######.#  
      #####  B    ###.#  
    BC...##  C    ###.#  
      ##.##       ###.#  
      ##...DE  F  ###.#  
      #####    G  ###.#  
      #########.#####.#  
    DE..#######...###.#  
      #.#########.###.#  
    FG..#########.....#  
      ###########.#####  
                 Z       
                 Z       

This map of the maze shows solid walls (#) and open passages (.). Every maze on Pluto has a start (the open tile
next to AA) and an end (the open tile next to ZZ). Mazes on Pluto also have portals; this maze has three pairs of
portals: BC, DE, and FG. When on an open tile next to one of these labels, a single step can take you to the
other tile with the same label. (You can only walk on . tiles; labels and empty space are not traversable.)

One path through the maze doesn't require any portals. Starting at AA, you could go down 1, right 8, down 12,
left 4, and down 1 to reach ZZ, a total of 26 steps.

However, there is a shorter path: You could walk from AA to the inner BC portal (4 steps), warp to the outer
BC portal (1 step), walk to the inner DE (6 steps), warp to the outer DE (1 step), walk to the outer FG (4 steps),
warp to the inner FG (1 step), and finally walk to ZZ (6 steps). In total, this is only 23 steps.

Here is a larger example:

                       A               
                       A               
      #################.#############  
      #.#...#...................#.#.#  
      #.#.#.###.###.###.#########.#.#  
      #.#.#.......#...#.....#.#.#...#  
      #.#########.###.#####.#.#.###.#  
      #.............#.#.....#.......#  
      ###.###########.###.#####.#.#.#  
      #.....#        A   C    #.#.#.#  
      #######        S   P    #####.#  
      #.#...#                 #......VT
      #.#.#.#                 #.#####  
      #...#.#               YN....#.#  
      #.###.#                 #####.#  
    DI....#.#                 #.....#  
      #####.#                 #.###.#  
    ZZ......#               QG....#..AS
      ###.###                 #######  
    JO..#.#.#                 #.....#  
      #.#.#.#                 ###.#.#  
      #...#..DI             BU....#..LF
      #####.#                 #.#####  
    YN......#               VT..#....QG
      #.###.#                 #.###.#  
      #.#...#                 #.....#  
      ###.###    J L     J    #.#.###  
      #.....#    O F     P    #.#...#  
      #.###.#####.#.#####.#####.###.#  
      #...#.#.#...#.....#.....#.#...#  
      #.#####.###.###.#.#.#########.#  
      #...#.#.....#...#.#.#.#.....#.#  
      #.###.#####.###.###.#.#.#######  
      #.#.........#...#.............#  
      #########.###.###.#############  
               B   J   C               
               U   P   P               

Here, AA has no direct path to ZZ, but it does connect to AS and CP. By passing through AS, QG, BU, and JO, you
can reach ZZ in 58 steps.

In your maze, how many steps does it take to get from the open tile marked AA to the open tile marked ZZ?
-}

data Cell = Wall | Empty | Portal String Char InOut | Blank Char | Endpoint String Char deriving (Show, Eq, Ord)
data InOut = Inner | Outer deriving (Show, Eq, Ord)

isPortal (Portal _ _ _) = True
isPortal _ = False
isStart (Endpoint "AA" _) = True
isStart _ = False
isEnd (Endpoint "ZZ" _) = True
isEnd _ = False
canEnter Empty = True
canEnter (Portal _ _ _) = True
canEnter (Endpoint _ _) = True
canEnter _ = False

isOuter (Portal _ _ Outer) = True
isOuter (Portal _ _ Inner) = False
isInner (Portal _ _ Outer) = True
isInner (Portal _ _ Inner) = False

(!) :: Maze -> Coord -> Cell
Maze { tiles=t } ! xy = t Map.! xy

type Coord = (Integer, Integer)
data Maze = Maze { tiles :: Map.Map Coord Cell
                 , portals :: Map.Map Coord Coord
                 }
  deriving (Eq)

instance Show Maze where
  show Maze { tiles=t } = unlines $ drawMapWith mazeCell t
    where
      mazeCell xy Nothing = '-'
      mazeCell xy (Just Wall) = '#'
      mazeCell xy (Just Empty) = '.'
      mazeCell xy (Just (Blank c)) = c
      mazeCell xy (Just (Portal (x:_) c _)) = c
      mazeCell xy (Just (Endpoint (x:_) c)) = c

parse ls =
  let m = [((x, y), c) | (y, line) <- [0..] `zip` ls, (x, c) <- [0..] `zip` line] & Map.fromList
      t = Map.mapWithKey (transform m) m
      p = t
        & Map.filter isPortal
        & Map.mapWithKey (\c (Portal s _ _) -> s)
        & mapReverse
        & Map.toList 
        & concatMap (\(_, s) -> let m = Set.findMin s; n = Set.findMax s in [(m, n), (n, m)])
        & Map.fromList
      
  in  Maze { tiles=t, portals=p }
  where
    maxX = ls & map length & maximum & pred & toInteger   {- inclusive maxima -}
    maxY = ls & length & pred & toInteger
    transform _ _ ' ' = Blank ' '
    transform _ _ '#' = Wall
    transform _ _ '.' = Empty
    transform m (x,y) c
      | isUpper c && Map.lookup (x, y+1) m == Just '.' = portalOrEnd (m Map.! (x, y-1)) c c (x,y)
      | isUpper c && Map.lookup (x+1, y) m == Just '.' = portalOrEnd (m Map.! (x-1, y)) c c (x,y)
      | isUpper c && Map.lookup (x, y-1) m == Just '.' = portalOrEnd c (m Map.! (x, y+1)) c (x,y)
      | isUpper c && Map.lookup (x-1, y) m == Just '.' = portalOrEnd c (m Map.! (x+1, y)) c (x,y)
      | isUpper c = Blank $ toLower c
    portalOrEnd 'A' 'A' _ _ = Endpoint "AA" '@'
    portalOrEnd 'Z' 'Z' _ _ = Endpoint "ZZ" '@'
    portalOrEnd a b c xy =
      let io = innerOrOuter xy
          c' = if io == Outer then c else toLower c
      in  Portal (a:[b]) c' io
    innerOrOuter (x,y)
      | x == 1 || x == (maxX - 1) = Outer
      | y == 1 || y == (maxY - 1) = Outer
      | otherwise = Inner
        

day20 ls =
  let maze = parse ls
      start = maze & tiles & Map.filter isStart & Map.keysSet & Set.findMin & adjacentEmpty maze
      end = maze & tiles & Map.filter isEnd & Map.keysSet & Set.findMin & adjacentEmpty maze
      Just path = flood maze start end
  in  (maze, portals maze, start, end, length path, path)


flood :: Maze -> Coord -> Coord -> Maybe [Set.Set Coord]
flood maze start end = flood0 [Set.singleton start] (Set.singleton start)
  where
    flood0 :: [Set.Set Coord] -> Set.Set Coord -> Maybe [Set.Set Coord]
    flood0 (p:ps) v =
        let horizon = Set.unions (Set.map (adjacent maze) p) `Set.difference` v
        in  if Set.null horizon then Nothing
            else if Set.member end horizon then Just (p:ps)
            else flood0 (horizon:p:ps) (Set.union horizon v)


adjacent :: Maze -> Coord -> Set.Set Coord
adjacent maze (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
                    & filter (\xy -> canEnter $ maze ! xy)
                    & map (\(x,y) ->
                            case maze ! (x,y) of
                              Portal _ _ _ ->
                                let (x',y') = (portals maze) Map.! (x,y)
                                in  adjacentEmpty maze (x',y')
                              otherwise -> (x,y)
                          )
                    & Set.fromList

adjacentEmpty maze (x,y) =
  let [xy] = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)] & filter (\xy -> maze ! xy == Empty)
  in  xy


reverseFlood maze end [] = []          
reverseFlood maze end (p:ps) =
  let [next] = p & Set.intersection (adjacent maze end) & Set.toList
  in  [end] ++ reverseFlood maze next ps           

{-
Strangely, the exit isn't open when you reach it. Then, you remember: the ancient Plutonians were famous for
 building recursive spaces.

The marked connections in the maze aren't portals: they physically connect to a larger or smaller copy of the
maze. Specifically, the labeled tiles around the inside edge actually connect to a smaller copy of the same maze,
and the smaller copy's inner labeled tiles connect to yet a smaller copy, and so on.

When you enter the maze, you are at the outermost level; when at the outermost level, only the outer labels AA
and ZZ function (as the start and end, respectively); all other outer labeled tiles are effectively walls. At any
other level, AA and ZZ count as walls, but the other outer labeled tiles bring you one level outward.

Your goal is to find a path through the maze that brings you back to ZZ at the outermost level of the maze.

In the first example above, the shortest path is now the loop around the right side. If the starting level is 0,
then taking the previously-shortest path would pass through BC (to level 1), DE (to level 2), and FG (back to
level 1). Because this is not the outermost level, ZZ is a wall, and the only option is to go back around to BC,
which would only send you even deeper into the recursive maze.

In the second example above, there is no path that brings you to ZZ at the outermost level.

Here is a more interesting example:

             Z L X W       C                 
             Z P Q B       K                 
  ###########.#.#.#.#######.###############  
  #...#.......#.#.......#.#.......#.#.#...#  
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  
  #.#...#.#.#...#.#.#...#...#...#.#.......#  
  #.###.#######.###.###.#.###.###.#.#######  
  #...#.......#.#...#...#.............#...#  
  #.#########.#######.#.#######.#######.###  
  #...#.#    F       R I       Z    #.#.#.#  
  #.###.#    D       E C       H    #.#.#.#  
  #.#...#                           #...#.#  
  #.###.#                           #.###.#  
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#  
CJ......#                           #.....#  
  #######                           #######  
  #.#....CK                         #......IC
  #.###.#                           #.###.#  
  #.....#                           #...#.#  
  ###.###                           #.#.#.#  
XF....#.#                         RF..#.#.#  
  #####.#                           #######  
  #......CJ                       NM..#...#  
  ###.#.#                           #.###.#  
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#  
  #.....#        F   Q       P      #.#.#.#  
  ###.###########.###.#######.#########.###  
  #.....#...#.....#.......#...#.....#.#...#  
  #####.#.###.#######.#######.###.###.#.#.#  
  #.......#.......#.#.#.#.#...#...#...#.#.#  
  #####.###.#####.#.#.#.#.###.###.#.###.###  
  #.......#.....#.#...#...............#...#  
  #############.#.#.###.###################  
               A O F   N                     
               A A D   M                     

One shortest path through the maze is the following:

    Walk from AA to XF (16 steps)
    Recurse into level 1 through XF (1 step)
    Walk from XF to CK (10 steps)
    Recurse into level 2 through CK (1 step)
    Walk from CK to ZH (14 steps)
    Recurse into level 3 through ZH (1 step)
    Walk from ZH to WB (10 steps)
    Recurse into level 4 through WB (1 step)
    Walk from WB to IC (10 steps)
    Recurse into level 5 through IC (1 step)
    Walk from IC to RF (10 steps)
    Recurse into level 6 through RF (1 step)
    Walk from RF to NM (8 steps)
    Recurse into level 7 through NM (1 step)
    Walk from NM to LP (12 steps)
    Recurse into level 8 through LP (1 step)
    Walk from LP to FD (24 steps)
    Recurse into level 9 through FD (1 step)
    Walk from FD to XQ (8 steps)
    Recurse into level 10 through XQ (1 step)
    Walk from XQ to WB (4 steps)
    Return to level 9 through WB (1 step)
    Walk from WB to ZH (10 steps)
    Return to level 8 through ZH (1 step)
    Walk from ZH to CK (14 steps)
    Return to level 7 through CK (1 step)
    Walk from CK to XF (10 steps)
    Return to level 6 through XF (1 step)
    Walk from XF to OA (14 steps)
    Return to level 5 through OA (1 step)
    Walk from OA to CJ (8 steps)
    Return to level 4 through CJ (1 step)
    Walk from CJ to RE (8 steps)
    Return to level 3 through RE (1 step)
    Walk from RE to IC (4 steps)
    Recurse into level 4 through IC (1 step)
    Walk from IC to RF (10 steps)
    Recurse into level 5 through RF (1 step)
    Walk from RF to NM (8 steps)
    Recurse into level 6 through NM (1 step)
    Walk from NM to LP (12 steps)
    Recurse into level 7 through LP (1 step)
    Walk from LP to FD (24 steps)
    Recurse into level 8 through FD (1 step)
    Walk from FD to XQ (8 steps)
    Recurse into level 9 through XQ (1 step)
    Walk from XQ to WB (4 steps)
    Return to level 8 through WB (1 step)
    Walk from WB to ZH (10 steps)
    Return to level 7 through ZH (1 step)
    Walk from ZH to CK (14 steps)
    Return to level 6 through CK (1 step)
    Walk from CK to XF (10 steps)
    Return to level 5 through XF (1 step)
    Walk from XF to OA (14 steps)
    Return to level 4 through OA (1 step)
    Walk from OA to CJ (8 steps)
    Return to level 3 through CJ (1 step)
    Walk from CJ to RE (8 steps)
    Return to level 2 through RE (1 step)
    Walk from RE to XQ (14 steps)
    Return to level 1 through XQ (1 step)
    Walk from XQ to FD (8 steps)
    Return to level 0 through FD (1 step)
    Walk from FD to ZZ (18 steps)

This path takes a total of 396 steps to move from AA at the outermost layer to ZZ at the outermost layer.

In your maze, when accounting for recursion, how many steps does it take to get from the open tile marked AA
to the open tile marked ZZ, both at the outermost layer?
-}

type Coord3 = (Integer, Integer, Integer) 

day20b ls =
  let maze = parse ls
      (x0,y0) = maze & tiles & Map.filter isStart & Map.keysSet & Set.findMin & adjacentEmpty maze
      start = (x0, y0, 0)
      (x1,y1) = maze & tiles & Map.filter isEnd & Map.keysSet & Set.findMin & adjacentEmpty maze
      end = (x1, y1, 0)
      path = flood3 maze start end
      len = if isJust path then fromJust path & length else -1
  in  (maze, portals maze, start, end, len, path)

flood3 :: Maze -> Coord3 -> Coord3 -> Maybe [Set.Set Coord3]
flood3 maze start end = flood0 [Set.singleton start] (Set.singleton start)
  where
    maxDepth = maze & portals & Map.size & toInteger
    flood0 :: [Set.Set Coord3] -> Set.Set Coord3 -> Maybe [Set.Set Coord3]
    flood0 (p:ps) v =
        let horizon = Set.unions (Set.map (adjacent3 maze maxDepth) p) `Set.difference` v
        in  if Set.null horizon then Nothing
            else if Set.member end horizon then Just (p:ps)
            else flood0 (horizon:p:ps) (Set.union horizon v)


adjacent3 :: Maze -> Integer -> Coord3 -> Set.Set Coord3
adjacent3 maze mz (x,y,z) = [(x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z)]
                    & filter (\(x,y,z) -> canEnter3 (x, y, z))
                    & map (\(x,y,z) ->
                            case maze ! (x,y) of
                              Portal _ _ Inner ->
                                let (x',y') = (portals maze) Map.! (x,y)
                                    (x'', y'') = adjacentEmpty maze (x',y')
                                in  (x'', y'', succ z)
                              Portal _ _ Outer ->
                                let (x',y') = (portals maze) Map.! (x,y)
                                    (x'', y'') = adjacentEmpty maze (x',y')
                                in  (x'', y'', pred z)
                              otherwise -> (x,y, z)
                          )
                    & Set.fromList
  where
    canEnter3 (x,y,z) =
      canEnter (maze ! (x,y)) &&
        case maze ! (x,y) of
          Endpoint _ _ -> z == 0
          Portal _ _ Inner -> z < mz
          Portal _ _ Outer -> z > 0
          otherwise -> True
