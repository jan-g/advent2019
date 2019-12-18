module Day18 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (isJust)
import Data.Sort (sortOn)
import qualified Data.Heap as Heap

import Control.Monad
import System.IO (hFlush, stdout)

import Lib
import Data.List.Extra (minimumOn)

{-
--- Day 18: Many-Worlds Interpretation ---

As you approach Neptune, a planetary security system detects you and activates a giant tractor beam on Triton! You have no choice but to land.

A scan of the local area reveals only one interesting feature: a massive underground vault. You generate a map of the tunnels (your puzzle input). The tunnels are too narrow to move diagonally.

Only one entrance (marked @) is present among the open passages (marked .) and stone walls (#), but you also detect an assortment of keys (shown as lowercase letters) and doors (shown as uppercase letters). Keys of a given letter open the door of the same letter: a opens A, b opens B, and so on. You aren't sure which key you need to disable the tractor beam, so you'll need to collect all of them.

For example, suppose you have the following map:

    #########
    #b.A.@.a#
    #########

Starting from the entrance (@), you can only access a large door (A) and a key (a). Moving toward the door doesn't help you, but you can move 2 steps to collect the key, unlocking A in the process:

    #########
    #b.....@#
    #########

Then, you can move 6 steps to collect the only other key, b:

    #########
    #@......#
    #########

So, collecting every key took a total of 8 steps.

Here is a larger example:

    ########################
    #f.D.E.e.C.b.A.@.a.B.c.#
    ######################.#
    #d.....................#
    ########################

The only reasonable move is to take key a and unlock door A:

    ########################
    #f.D.E.e.C.b.....@.B.c.#
    ######################.#
    #d.....................#
    ########################

Then, do the same with key b:

    ########################
    #f.D.E.e.C.@.........c.#
    ######################.#
    #d.....................#
    ########################

...and the same with key c:

    ########################
    #f.D.E.e.............@.#
    ######################.#
    #d.....................#
    ########################

Now, you have a choice between keys d and e. While key e is closer, collecting it now would be slower in the long run than collecting key d first, so that's the best choice:

    ########################
    #f...E.e...............#
    ######################.#
    #@.....................#
    ########################

Finally, collect key e to unlock door E, then collect key f, taking a grand total of 86 steps.

Here are a few more examples:

    ########################
    #...............b.C.D.f#
    #.######################
    #.....@.a.B.c.d.A.e.F.g#
    ########################

    Shortest path is 132 steps: b, a, c, d, f, e, g

    #################
    #i.G..c...e..H.p#
    ########.########
    #j.A..b...f..D.o#
    ########@########
    #k.E..a...g..B.n#
    ########.########
    #l.F..d...h..C.m#
    #################

    Shortest paths are 136 steps;
    one is: a, f, b, j, g, n, h, d, l, o, e, p, c, i, k, m

    ########################
    #@..............ac.GI.b#
    ###d#e#f################
    ###A#B#C################
    ###g#h#i################
    ########################

    Shortest paths are 81 steps; one is: a, c, f, i, d, g, b, e, h

How many steps is the shortest path that collects all of the keys?
-}

type Coord = (Integer, Integer)

type Maze = Map.Map Coord Char

parse ls =
  [((x, y), c) | (y, line) <- [0..] `zip` ls, (x, c) <- [0..] `zip` line] & Map.fromList


mazeDraw m =
  drawMapWith mazeChar m & unlines
  where
    mazeChar p (Just c) = c

{-
You arrive at the vault only to discover that there is not one vault, but four - each with its own entrance.

On your map, find the area in the middle that looks like this:

    ...
    .@.
    ...

Update your map to instead use the correct data:

    @#@
    ###
    @#@

This change will split your map into four separate sections, each with its own entrance:

    #######       #######
    #a.#Cd#       #a.#Cd#
    ##...##       ##@#@##
    ##.@.##  -->  #######
    ##...##       ##@#@##
    #cB#Ab#       #cB#Ab#
    #######       #######

Because some of the keys are for doors in other vaults, it would take much too long to collect all of the keys
by yourself. Instead, you deploy four remote-controlled robots. Each starts at one of the entrances (@).

Your goal is still to collect all of the keys in the fewest steps, but now, each robot has its own position
and can move independently. You can only remotely control a single robot at a time. Collecting a key instantly
unlocks any corresponding doors, regardless of the vault in which the key or door is found.

For example, in the map above, the top-left robot first collects key a, unlocking door A in the bottom-right vault:

    #######
    #@.#Cd#
    ##.#@##
    #######
    ##@#@##
    #cB#.b#
    #######

Then, the bottom-right robot collects key b, unlocking door B in the bottom-left vault:

    #######
    #@.#Cd#
    ##.#@##
    #######
    ##@#.##
    #c.#.@#
    #######

Then, the bottom-left robot collects key c:

    #######
    #@.#.d#
    ##.#@##
    #######
    ##.#.##
    #@.#.@#
    #######

Finally, the top-right robot collects key d:

    #######
    #@.#.@#
    ##.#.##
    #######
    ##.#.##
    #@.#.@#
    #######

In this example, it only took 8 steps to collect all of the keys.

Sometimes, multiple robots might have keys available, or a robot might have to wait for multiple keys to be
collected:

    ###############
    #d.ABC.#.....a#
    ######@#@######
    ###############
    ######@#@######
    #b.....#.....c#
    ###############

First, the top-right, bottom-left, and bottom-right robots take turns collecting keys a, b, and c, a total of
6 + 6 + 6 = 18 steps. Then, the top-left robot can access key d, spending another 6 steps; collecting all of
the keys here takes a minimum of 24 steps.

Here's a more complex example:

    #############
    #DcBa.#.GhKl#
    #.###@#@#I###
    #e#d#####j#k#
    ###C#@#@###J#
    #fEbA.#.FgHi#
    #############

    Top-left robot collects key a.
    Bottom-left robot collects key b.
    Top-left robot collects key c.
    Bottom-left robot collects key d.
    Top-left robot collects key e.
    Bottom-left robot collects key f.
    Bottom-right robot collects key g.
    Top-right robot collects key h.
    Bottom-right robot collects key i.
    Top-right robot collects key j.
    Bottom-right robot collects key k.
    Top-right robot collects key l.

In the above example, the fewest steps to collect all of the keys is 32.

Here's an example with more choices:

    #############
    #g#f.D#..h#l#
    #F###e#E###.#
    #dCba@#@BcIJ#
    #############
    #nK.L@#@G...#
    #M###N#H###.#
    #o#m..#i#jk.#
    #############

One solution with the fewest steps is:

    Top-left robot collects key e.
    Top-right robot collects key h.
    Bottom-right robot collects key i.
    Top-left robot collects key a.
    Top-left robot collects key b.
    Top-right robot collects key c.
    Top-left robot collects key d.
    Top-left robot collects key f.
    Top-left robot collects key g.
    Bottom-right robot collects key k.
    Bottom-right robot collects key j.
    Top-right robot collects key l.
    Bottom-left robot collects key n.
    Bottom-left robot collects key m.
    Bottom-left robot collects key o.

This example requires at least 72 steps to collect all keys.

After updating your map and using the remote-controlled robots, what is the fewest steps necessary to collect
all of the keys?
-}

-- xdfsqzbvlyampehjgwtoucnrki@ 4118


day18 ls =
  let maze = parse ls
      {- Map (from, to) -> (distance, {doors} -}
      keys = Map.filter (\c -> isLower c || c == '@') maze & Map.toList
      paths = [((from, to), (d, doors))
               | ((fx, fy), from) <- keys,
                 ((tx, ty), to) <- keys,
                 let path = flood maze (fx, fy) (tx, ty),
                 isJust path,
                 let Just (d, doors) = path] & Map.fromList
      hunt = bfsHuntPaths paths '@'
  in  hunt


{- Flood-trace (BFS) a path from a starting to finishing coordinate
   Don't walk over walls; everything else is passable - although we also return
   the set of "doors" (ie, A B C etc) we've moved over. This is coerced to
   LC to match up with the set of keys. -}
flood :: Map.Map Coord Char -> Coord -> Coord -> Maybe (Int, Set.Set Char)
flood tiles from to =
  case flood0 [Set.singleton from] Set.empty of
    Nothing -> Nothing
    Just path -> Just (length path, map (tiles Map.!) path & filter isUpper & Set.fromList & Set.map toLower)
  where
    flood0 (p:ps) visited =
      if Set.member to p
      then Just (backtrack to ps)
      else
        let next = Set.unions (Set.map (\xy -> neighs xy) p) `Set.difference` visited
        in  if Set.null next then Nothing
            else
              let visited' = visited `Set.union` next
              in  flood0 (next:p:ps) visited'
    neighs (x, y) =
      [(x-1, y), (x+1, y), (x, y-1), (x, y+1)] & filter (\xy -> tiles Map.! xy /= '#') & Set.fromList
    adjacent (x, y) (x2,y2) = abs(x2 - x) + abs(y2 - y) == 1
    backtrack :: Coord -> [Set.Set Coord] -> [Coord]
    backtrack _ [] = []
    backtrack to (p:ps) =
      let prev = (Set.filter (adjacent to) p & Set.findMin)
      in  prev : (backtrack prev ps)


bfsHuntPaths :: Map.Map (Char, Char) (Int, Set.Set Char) -> Char -> Int
bfsHuntPaths paths start =
  bfsHunt0 (Heap.singleton (0, ('@', Set.singleton '@'))) Set.empty
  where
    allNodes = Map.keysSet paths & Set.toList & map fst & Set.fromList
    bfsHunt0 :: Heap.MinPrioHeap Int (Char, Set.Set Char) -> Set.Set (Char, Set.Set Char) -> Int
    bfsHunt0 pq seen =
      let Just ((steps, (at, visited)), pq') = Heap.view pq
--          putStrLn $ "Visiting: " ++ (show steps) ++ " " ++ (show at) ++ " : " ++ (show visited)
      in  if Set.size visited == Set.size allNodes
          then steps
          else if Set.member (at, visited) seen
          then bfsHunt0 pq' seen
          else
            {- Compute neighbors reachable right now -}
            let seen' = Set.insert (at, visited) seen
                ns = pathReachable at visited
                   & filter (\(dist, dest) -> not $ Set.member dest visited)
                pq'' = foldl (\pq (steps', dest) ->
                               let visited' = Set.insert dest visited
                               in  if Set.member (dest, visited') seen'
                                   then pq
                                   else Heap.insert (steps + steps', (dest, visited')) pq
                             ) pq' ns
            in  bfsHunt0 pq'' seen'
    pathReachable :: Char -> Set.Set Char -> [(Int, Char)]
    pathReachable from visited =
      paths & Map.filterWithKey (\(f, t) (steps, blockedBy) -> f == from
                                                            && Set.null (blockedBy `Set.difference` visited))
            & Map.toList
            & map (\((_, dest),(dist, _)) -> (dist, dest))


day18b ls = do
   let maze = parse ls :: Maze

   {- update the maze -}
       (x, y) = pos maze '@' & Set.toList & head
       t = [((x-1, y-1), '1'), ((x  , y-1), '#'), ((x+1, y-1), '2'),
            ((x-1, y  ), '#'), ((x  , y  ), '#'), ((x+1, y  ), '#'),
            ((x-1, y+1), '3'), ((x  , y+1), '#'), ((x+1, y+1), '4')] & Map.fromList
       maze' = Map.union t maze
       pos' = Set.fromList "1234"
       
       keys = Map.filter (\c -> isLower c || isDigit c) maze' & Map.toList
       paths = [((from, to), (d, doors))
                | ((fx, fy), from) <- keys,
                  ((tx, ty), to) <- keys,
                  not $ isDigit to,
                  let path = flood maze' (fx, fy) (tx, ty),
                  isJust path,
                  let Just (d, doors) = path] & Map.fromList

   putStrLn $ mazeDraw maze'
   putStrLn $ "keys are " ++ (show keys)
   putStrLn $ show $ Map.filterWithKey (\(f,t) v -> isDigit f) paths
   putStrLn $ show $ paths
   steps <- bfsHuntMulti paths pos'
   return steps


pos maze c = maze & Map.filter (==c) & Map.keysSet


bfsHuntMulti :: Map.Map (Char, Char) (Int, Set.Set Char) -> Set.Set Char -> IO (Int, String)
bfsHuntMulti paths positions = do
  bfsHuntMulti0 (Heap.singleton (0, (positions, Set.empty, ""))) Set.empty 0
  where
    allNodes = Map.keysSet paths & Set.toList & map fst & filter isLower & Set.fromList
    bfsHuntMulti0 :: Heap.MinPrioHeap Int (Set.Set Char, Set.Set Char, String)  {- positions, acquired, order -}
             -> Set.Set (Set.Set Char, Set.Set Char)  {- states visited: positions of droids, keys held -}
             -> Int  {- reporting depth -}
             -> IO (Int, [Char])  {- step count plus collection order -}
    bfsHuntMulti0 pq reached report = do
          let Just ((steps, (locations, collected, path)), pq') = Heap.view pq
          report' <- if steps > report then do
                       putStrLn $ show steps
                       hFlush stdout
                       return $ report + 100
                     else return report

          if Set.member (locations, collected) reached
          then do
--            putStrLn $ "Skipping after " ++ (show steps) ++ " with found: " ++ path
            bfsHuntMulti0 pq' reached report'
          else if Set.size collected == Set.size allNodes
          then do
            putStrLn $ "Returning solution after " ++ (show steps) ++ " with found: " ++ path
            return $ (steps, path)
          else do
--            putStrLn $ "Expanding after " ++ (show steps) ++ " steps, with found keys: " ++ path
            let reached' = Set.insert (locations, collected) reached
                pq'' = Set.foldl (\pq pos ->
                         let locations' = Set.delete pos locations
                             ns = pathReachable pos collected :: [(Int, Char)]
                         in  foldl (\pq (steps', pos') ->
                               let collected' = Set.insert pos' collected
                                   locations'' = Set.insert pos' locations'
                               in  if Set.member (locations'', collected') reached'
                                   then pq
                                   else Heap.insert (steps + steps', (locations'', collected', pos' : path)) pq
                             ) pq ns
                       ) pq' locations
              
            bfsHuntMulti0 pq'' reached' report'
    pathReachable :: Char -> Set.Set Char -> [(Int, Char)]
    pathReachable from visited =
      paths & Map.filterWithKey (\(f, t) (steps, blockedBy) -> f == from
                                                            && not (Set.member t visited)
                                                            && Set.null (blockedBy `Set.difference` visited))
            & Map.toList
            & map (\((_, dest),(dist, _)) -> (dist, dest))
