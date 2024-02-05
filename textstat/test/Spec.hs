-- reference : https://www.youtube.com/watch?v=PGsDvgmZF7A
import Test.Hspec
import Control.Exception (evaluate)
import Stats
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
  describe "Test : calcCaracteres" $ do
    it "Should be 0" $ do
        calcCaracteres (T.pack "") `shouldBe` 0
    it "Should be 3" $ do
        calcCaracteres (T.pack "Hichem") `shouldBe` 6
  describe "Test : calcMots" $ do
    it "Should be 0" $ do
        calcMots (T.pack "") `shouldBe` 0
    it "Should be 2" $ do
        calcMots (T.pack "Lorem Ipsum Marin Jaging free") `shouldBe` 5
  describe "Test : meilleurFrequence" $ do
    it "Should be 0" $ do
        meilleurFrequence (T.pack "") 'h' `shouldBe` 0
    it "Should be 1" $ do
        meilleurFrequence (T.pack "Lorem Ipsum Marin Jaging free") 'e' `shouldBe` 3