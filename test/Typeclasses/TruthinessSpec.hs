module Typeclasses.TruthinessSpec() where

import           Test.Hspec
import           Typeclasses.Truthiness (Truthiness (..),  evalTruthiness )
import           Typeclasses.BinaryTreeFunctor (BinaryTree (..), treeInsert)
import           Typeclasses.Custom(TrafficLight(..))


main :: IO ()
main =
  hspec $ do
    describe ">>>>>> Evaluate Truthiness <<<<<<" $ do
      it "truthiness of \"\" `shouldBe` False" $ do        
       evalTruthiness "" `shouldBe` False
      it "truthiness of [] `shouldBe` False" $ do        
       evalTruthiness [] `shouldBe` False
      it "truthiness of True `shouldBe` True" $ do        
       evalTruthiness True `shouldBe` True
      it "truthiness of (Just 0)`shouldBe` True" $ do        
       evalTruthiness (Just 0) `shouldBe` True
      it "truthiness of Empty `shouldBe` False" $ do        
       evalTruthiness Empty `shouldBe` False
      it "truthiness of Orange `shouldBe` True" $ do        
       evalTruthiness Yellow `shouldBe` True
      it "truthiness of 0 `shouldBe` False" $ do        
       evalTruthiness (0::Int) `shouldBe` False
