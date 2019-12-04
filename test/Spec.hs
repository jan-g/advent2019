import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))

import qualified Day2
import qualified Day3
import qualified Day4


main :: IO ()
main = hspec $ do
  describe "Day2" $ do
    it "correctly runs 1,0,0,0,99" $ do
      (Day2.run 0 (Day2.parse "1,0,0,0,99") & elems) `shouldBe` [2,0,0,0,99]
    it "correctly runs 2,3,0,3,99" $ do
      (Day2.run 0 (Day2.parse "2,3,0,3,99") & elems) `shouldBe` [2, 3, 0, 6, 99]
    it "correctly runs 2,4,4,5,99,0" $ do
      (Day2.run 0 (Day2.parse "2,4,4,5,99,0") & elems) `shouldBe` [2,4,4,5,99,9801]
    it "correctly runs 1,1,1,4,99,5,6,0,99" $ do
      (Day2.run 0 (Day2.parse "1,1,1,4,99,5,6,0,99") & elems) `shouldBe` [30,1,1,4,2,5,6,0,99]

  describe "Day3" $ do
    it "runs the first example" $ do
      Day3.day3 ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                ,"U62,R66,U55,R34,D71,R55,D58,R83"
                ] `shouldBe` 159
    it "runs the second example" $ do
      Day3.day3 ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                ,"U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                ] `shouldBe` 135

  describe "Day3b" $ do
    it "runs the first example" $ do
      Day3.day3b ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                 ,"U62,R66,U55,R34,D71,R55,D58,R83"
                 ] `shouldBe` 610
    it "runs the second example" $ do
      Day3.day3b ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                 ,"U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                 ] `shouldBe` 410

  describe "Day4" $ do
    it "checks 111111" $ do
      Day4.criterion 111111 `shouldBe` True
    it "checks 223450" $ do
      Day4.criterion 223450 `shouldBe` False
    it "checks 123789" $ do
      Day4.criterion 123789 `shouldBe` False

  describe "Day4b" $ do
    it "checks 112233" $ do
      Day4.criterionb 112233 `shouldBe` True
    it "checks 123444" $ do
      Day4.criterionb 123444 `shouldBe` False
    it "checks 111122" $ do
      Day4.criterionb 111122 `shouldBe` True