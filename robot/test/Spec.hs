import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Robot" $ do
    it "should return the good number of possibilities" $ do
      robot 3 [[0, 0], [2, 0], [0, 0], [0, 1]] `shouldBe` 4

    it "should return the good number of possibilities for the second case"   $ do
      robot 0 [[1,10],[1,10],[1,10],[1,10]] `shouldBe` 8