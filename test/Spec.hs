import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))

import qualified Day2


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
