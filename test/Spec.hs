import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))
import qualified Data.Map.Strict as Map

import qualified Day2
import qualified Day3
import qualified Day4
import qualified Intcode
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15


main :: IO ()
main =
  hspec $ do
    describe "Day2" $ do
      it "correctly runs 1,0,0,0,99" $ (Day2.run 0 (Day2.parse "1,0,0,0,99") & elems) `shouldBe` [2, 0, 0, 0, 99]
      it "correctly runs 2,3,0,3,99" $ (Day2.run 0 (Day2.parse "2,3,0,3,99") & elems) `shouldBe` [2, 3, 0, 6, 99]
      it "correctly runs 2,4,4,5,99,0" $
        (Day2.run 0 (Day2.parse "2,4,4,5,99,0") & elems) `shouldBe` [2, 4, 4, 5, 99, 9801]
      it "correctly runs 1,1,1,4,99,5,6,0,99" $
        (Day2.run 0 (Day2.parse "1,1,1,4,99,5,6,0,99") & elems) `shouldBe` [30, 1, 1, 4, 2, 5, 6, 0, 99]

    describe "Day3" $ do
      it "runs the first example" $
        Day3.day3 ["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"] `shouldBe` 159
      it "runs the second example" $
        Day3.day3 ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"] `shouldBe` 135

    describe "Day3b" $
      it "runs the first example" $ do
        Day3.day3b ["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"] `shouldBe` 610
        Day3.day3b ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"] `shouldBe`
          410

    describe "Day4" $ do
      it "checks 111111" $ Day4.criterion 111111 `shouldBe` True
      it "checks 223450" $ Day4.criterion 223450 `shouldBe` False
      it "checks 123789" $ Day4.criterion 123789 `shouldBe` False

    describe "Day4b" $ do
      it "checks 112233" $ Day4.criterionb 112233 `shouldBe` True
      it "checks 123444" $ Day4.criterionb 123444 `shouldBe` False
      it "checks 111122" $ Day4.criterionb 111122 `shouldBe` True
      
    describe "Day5" $ do
     it "has working intcode" $ do
       let prog = Intcode.parse "3,0,4,0,99"
       (Intcode.run prog [6] & snd) `shouldBe` [6]

    describe "Day5b" $ do
      let run prog input = Intcode.run (Intcode.parse prog) input & snd
      it "does comparisons" $ do
        run "3,9,8,9,10,9,4,9,99,-1,8" [8] `shouldBe` [1]
        run "3,9,8,9,10,9,4,9,99,-1,8" [7] `shouldBe` [0]

        run "3,9,7,9,10,9,4,9,99,-1,8" [7] `shouldBe` [1]
        run "3,9,7,9,10,9,4,9,99,-1,8" [8] `shouldBe` [0]
        run "3,9,7,9,10,9,4,9,99,-1,8" [9] `shouldBe` [0]

        run "3,3,1108,-1,8,3,4,3,99" [9] `shouldBe` [0]
        run "3,3,1108,-1,8,3,4,3,99" [8] `shouldBe` [1]

        run "3,3,1107,-1,8,3,4,3,99" [8] `shouldBe` [0]
        run "3,3,1107,-1,8,3,4,3,99" [9] `shouldBe` [0]
        run "3,3,1107,-1,8,3,4,3,99" [7] `shouldBe` [1]

      it "does jt jumps" $ do
        run "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [0] `shouldBe` [0]
        run "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [1] `shouldBe` [1]
        run "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [-1] `shouldBe` [1]

      it "runs the longer example" $ do
        let prog = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\
                   \1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\
                   \999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
        run prog [7] `shouldBe` [999]
        run prog [8] `shouldBe` [1000]
        run prog [9] `shouldBe` [1001]

    describe "Day6" $ do
      it "calculates the examples" $ do
        let input = ["COM)B"
                    ,"B)C"
                    ,"C)D"
                    ,"D)E"
                    ,"E)F"
                    ,"B)G"
                    ,"G)H"
                    ,"D)I"
                    ,"E)J"
                    ,"J)K"
                    ,"K)L"
                    ]
            ot = Day6.parse input
        Day6.orbitSize ot "D" `shouldBe` 3
        Day6.orbitSize ot "L" `shouldBe` 7
        Day6.orbitSize ot "COM" `shouldBe` 0
    
    describe "Day6b" $ do
      it "works" $ do
        let input = ["COM)B"
                    ,"B)C"
                    ,"C)D"
                    ,"D)E"
                    ,"E)F"
                    ,"B)G"
                    ,"G)H"
                    ,"D)I"
                    ,"E)J"
                    ,"J)K"
                    ,"K)L"
                    ,"K)YOU"
                    ,"I)SAN"
                    ]        
            ot = Day6.parse input
        Day6.orbitalDistance ot "E" "I" `shouldBe` 2
        Day6.orbitalDistance ot "L" "E" `shouldBe` 3
        Day6.orbitalDistance ot "COM" "COM" `shouldBe` 0

    describe "Day7" $ do
      it "runs example 1" $ do
        Day7.day7 ["3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"] `shouldBe` 43210

      it "runs example 2" $ do
        Day7.day7 ["3,23,3,24,1002,24,10,24,1002,23,-1,23,\
                   \101,5,23,23,1,24,23,23,4,23,99,0,0"] `shouldBe` 54321

      it "runs example3" $ do
        Day7.day7 ["3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,\
                   \1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"] `shouldBe` 65210

    describe "Day7b" $ do
      it "does example 1" $ do
        Day7.day7b ["3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,\
                    \27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"] `shouldBe` 139629729

      it "does example 2" $ do
        Day7.day7b ["3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,\
                    \-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,\
                    \53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"] `shouldBe` 18216

    {-
    describe "Day8" $ do it "" $ do Day8.day8 [] `shouldBe` "hello world"
    describe "Day8b" $ do it "" $ do Day8.day8b [] `shouldBe` "hello world"
    -}
    
    describe "Day9" $ do
      it "runs a quine" $ do
        let input = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
            prog = Intcode.toProg input
        (Intcode.run prog [] & snd) `shouldBe` input
      it "outputs a 16-digit number" $ do
        let input = [1102,34915192,34915192,7,4,7,99,0]
            prog = Intcode.toProg input
        (Intcode.run prog [] & snd & head & show & length) `shouldBe` 16
      it "runs the third example" $ do
        let input = [104,1125899906842624,99]
            prog = Intcode.toProg input
        (Intcode.run prog [] & snd) `shouldBe` [input !! 1]


    describe "Day10" $ do
      it "works" $ do
        let input = [".#..#"
                    ,"....."
                    ,"#####"
                    ,"....#"
                    ,"...##"
                    ]
            stars = Day10.parse input
        Day10.day10 input `shouldBe` 8

      it "works on example 2" $ do
        let input = ["......#.#."
                    ,"#..#.#...."
                    ,"..#######."
                    ,".#.#.###.."
                    ,".#..#....."
                    ,"..#....#.#"
                    ,"#..#....#."
                    ,".##.#..###"
                    ,"##...#..#."
                    ,".#....####"
                    ]
        Day10.day10 input `shouldBe` 33


    describe "Day10b" $ do
      it "sorts alignment sets" $ do
        let coords = [(x, y) | x <- [0..2], y <- [0..2]] :: [Day10.Coord]
        Day10.sortedAlignmentSets (1, 1) coords `shouldBe` [[(0, -1)], [(1, -1)], [(1,0)],[(1,1)],[(0,1)],[(-1,1)],[(-1,0)],[(-1,-1)]]
        
      it "works" $ do
        let input = [".#..##.###...#######"
                    ,"##.############..##."
                    ,".#.######.########.#"
                    ,".###.#######.####.#."
                    ,"#####.##.#.##.###.##"
                    ,"..#####..#.#########"
                    ,"####################"
                    ,"#.####....###.#.#.##"
                    ,"##.#################"
                    ,"#####.##.###..####.."
                    ,"..######..##.#######"
                    ,"####.##.####...##..#"
                    ,".#####..#.######.###"
                    ,"##...#.####X#####..."
                    ,"#.##########.#######"
                    ,".####.#.###.###.#.##"
                    ,"....##.##.###..#####"
                    ,".#.#.###########.###"
                    ,"#.#.#.#####.####.###"
                    ,"###.##.####.##.#..##"
                    ]
            stars = Day10.parse input
            (sc, (x, y)) = Day10.scoreAndBestSpot stars
            as = Day10.sortedAlignmentSets (x, y) stars
        (x, y, sc) `shouldBe` (11, 13, 211) 

        Day10.cyclicTake 1 as `shouldBe` [(11 - x, 12 - y)]
        (as & Day10.cyclicDrop 1 & Day10.cyclicTake 1) `shouldBe` [(12 - x, 1 - y)]
        (as & Day10.cyclicDrop 2 & Day10.cyclicTake 1) `shouldBe` [(12 - x, 2 - y)]
        (as & Day10.cyclicDrop 9 & Day10.cyclicTake 1) `shouldBe` [(12 - x, 8 - y)]
        (as & Day10.cyclicDrop 19 & Day10.cyclicTake 1) `shouldBe` [(16 - x, 0 - y)]
        (as & Day10.cyclicDrop 49 & Day10.cyclicTake 1) `shouldBe` [(16 - x, 9 - y)]
        (as & Day10.cyclicDrop 99 & Day10.cyclicTake 1) `shouldBe` [(10 - x, 16 - y)]
        (as & Day10.cyclicDrop 198 & Day10.cyclicTake 1) `shouldBe` [(9 - x, 6 - y)]
        (as & Day10.cyclicDrop 199 & Day10.cyclicTake 1) `shouldBe` [(8 - x, 2 - y)]
        (as & Day10.cyclicDrop 200 & Day10.cyclicTake 1) `shouldBe` [(10 - x, 9 - y)]
        (as & Day10.cyclicDrop 298) `shouldBe` [[(11 - x, 1 - y)]]

    describe "Day11" $ do
      it "works" $ do
        let progOutputs = [1,0, 0,0, 1,0, 1,0, 0,1, 1,0, 1,0]
            expectedInputs = [0, 0, 0, 0, 1, 0, 0, 0]
            robot = Day11.zeroedRobot
            (inputs, robot') = Day11.runRobot robot progOutputs
        inputs `shouldBe` expectedInputs
        (robot' & Day11.hull & Day11.countPaint) `shouldBe` 6

    describe "Day11b" $ do
      it "works" $ do
        let progOutputs = [1,0, 0,0, 1,0, 1,0, 0,1, 1,0, 1,0]
            expectedInputs = [0, 0, 0, 0, 1, 0, 0, 0]
            robot = Day11.zeroedRobot
            (inputs, robot') = Day11.runRobot robot progOutputs
            result = Day11.paint $ Day11.hull robot'
        result `shouldBe` ["..#"
                          ,"..#"
                          ,"##."
                          ]

    describe "Day12" $ do
      let input = ["<x=-1, y=0, z=2>"
                  ,"<x=2, y=-10, z=-7>"
                  ,"<x=4, y=-8, z=8>"
                  ,"<x=3, y=5, z=-1>"]
          moons = Day12.parse input
          moons' = iterate Day12.update moons
      it "works" $ do

        moons `shouldBe` [Day12.moon0 (-1) 0 2
                            ,Day12.moon0 2 (-10) (-7)
                            ,Day12.moon0 4 (-8) 8
                            ,Day12.moon0 3 5 (-1)
                            ]         
        (head . drop 1) moons' `shouldBe` [Day12.moon (2, -1, 1) (3, -1, -1)
                                          ,Day12.moon (3, -7, -4) (1, 3, 3)
                                          ,Day12.moon (1, -7, 5) (-3, 1, -3)
                                          ,Day12.moon (2, 2, 0) (-1, -3, 1)
                                          ]         
      it "computes energy" $ do
        (moons' & drop 10 & head & map Day12.totalEnergy & sum) `shouldBe` 179

    describe "Day12b" $ do
      let xs = [(-1, 0), (2, 0), (4, 0), (3, 0)]
          ys = [(0, 0), (-10, 0), (-8, 0), (5, 0)]
          zs = [(2, 0), (-7, 0), (8, 0), (-1, 0)]
          xs' = iterate Day12.updateCoord xs
          ys' = iterate Day12.updateCoord ys
          zs' = iterate Day12.updateCoord zs
      it "works" $ do
        (xs' & drop 1 & head) `shouldBe` [(2, 3), (3, 1), (1, -3), (2, -1)]
        (ys' & drop 1 & head) `shouldBe` [(-1, -1), (-7, 3), (-7, 1), (2, -3)]

        let x0 = xs' & drop 10 & head
            y0 = ys' & drop 10 & head
            z0 = zs' & drop 10 & head
        Day12.totalEnergy' x0 y0 z0 `shouldBe` 179
        
        (xs' & drop 2772 & head) `shouldBe` xs
        (ys' & drop 2772 & head) `shouldBe` ys
        (zs' & drop 2772 & head) `shouldBe` zs
        
        let cx = Day12.findCycle xs'
            cy = Day12.findCycle ys'
            cz = Day12.findCycle zs'
        cx `lcm` cy `lcm` cz `shouldBe` 2772

{-
    describe "Day13" $ do
      it "works" $ do
        Day13.day13 [] `shouldBe` "hello world"
        
    describe "Day13b" $ do
      it "works" $ do
        Day13.day13b [] `shouldBe` "hello world"
-}

    describe "Day14b" $ do
      it "works" $ do
        let input = ["1 ORE => 2 A"
                    ,"1 A => 1 FUEL"]
            recipe = Day14.parse input
            reqMap = Day14.goesInto recipe
            want = Map.singleton "FUEL" 1
        Day14.search reqMap recipe 1000000 0 3000000 `shouldBe` 2000000

    describe "Day15" $ do
      it "searched" $ do
        let ship0 = Map.singleton (0, 0) Day15.empty
        Day15.pathTo ship0 (0, 0) Day15.unknown `shouldBe` [[(0,0),(-1,0)],[(0,0),(0,-1)],[(0,0),(0,1)],[(0,0),(1,0)]]

        let ship0 = Map.fromList [((0, 0), Day15.empty), ((1, 0), Day15.wall)]
        Day15.pathTo ship0 (0, 0) Day15.unknown `shouldBe` [[(0,0),(-1,0)],[(0,0),(0,-1)],[(0,0),(0,1)]]

        let ship1 = Map.fromList [((0, 0), Day15.empty)
                                 ,((1, 0), Day15.wall)
                                 ,((-1, 0), Day15.wall)
                                 ,((0, 1), Day15.wall)
                                 ]
        Day15.pathTo ship1 (0, 0) Day15.unknown `shouldBe` [[(0,0),(0,-1)]]

        let ship2 = Map.fromList [((0, 0), Day15.empty)
                                 ,((1, 0), Day15.wall)
                                 ,((-1, 0), Day15.wall)
                                 ,((0, 1), Day15.wall)
                                 ,((0, -1), Day15.empty)
                                 ,((0, -2), Day15.wall)
                                 ]
        Day15.pathTo ship2 (0, 0) Day15.unknown `shouldBe` [[(0,0),(0,-1),(-1,-1)],[(0,0),(0,-1),(1,-1)]]

        let ship3 = Map.fromList [((0, 0), Day15.empty)
                                 ,((-1, 0), Day15.wall)
                                 ,((0, 1), Day15.wall)
                                 ,((0, -1), Day15.empty)
                                 ,((0, -2), Day15.wall)
                                 ]
        Day15.pathTo ship3 (0, 0) Day15.unknown `shouldBe` [[(0,0),(1,0)]]

        let ship4 = Map.fromList $ [((x, -1), Day15.wall) | x <- [-100..100]] ++
                                   [((x, 1), Day15.wall) | x <- [-100..100]] ++
                                   [((x, 0), Day15.empty) | x <- [-100..100]] ++
                                   [((-101, 0), Day15.wall)]
        Day15.pathTo ship4 (-30, 0) Day15.unknown `shouldBe` [[(x, 0) | x <- [-30..101]]]


{-
    describe "Day15b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day16" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day16b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day17" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day17b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day18" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day18b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day19" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day19b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day20" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day20b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day21" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day21b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day22" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day22b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day23" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day23b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day24" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day24b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
-}
