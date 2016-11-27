import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Bee" $ do
    it "should return 1 for a one-letter word" $ do
      bee "a" `shouldBe` 1

    it "should return 4 for a two-letter word" $ do
      bee "ag" `shouldBe` 4
  
    it "should return the correct possibilities for a longer word" $ do
      bee "abcde" `shouldBe` 108

    it "should lower the number of possibilities if there are two identicals letters together" $ do
	  bee "aabcd" `shouldBe` 36

    it "should 1 if letters are identicals" $ do
      bee "aaaaa" `shouldBe` 1

    it "should be the number of possibilities mod 10^9 + 7" $ do
      bee "eeakiajjjdhegfihgfjajhfjhfiegieibcgjcefbfadjfkejcchajbjfehhkcjgdbbkbddaidefgbjedabchaakdfgaffggehgfihggkijdjffkdgbkfahidaihfechjgchcgabdhckbihebhbghibiikebbhgiciefbceejhbicjkdfakkgafciidhejfefhhgikjeekcggbjjagggfkichjkagccajieghcaaaagebcbkjhgdgddcaaagcbgkji" `shouldSatisfy`(\a -> a < 1000000007 )