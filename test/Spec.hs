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

{-
    describe "Day7" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day7b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day8" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day8b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day9" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day9b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day10" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day10b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day11" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day11b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day12" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day12b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day13" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day13b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day14" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day14b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day15" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
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
