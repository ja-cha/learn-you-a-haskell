module Typeclasses.WriterSpec() where

import           Test.Hspec
import           Typeclasses.Writer ( multiLogDo, multiLogBind, greatestCommonDenominator)
import Control.Monad.Writer

main :: IO ()
main =
  hspec $ do    
    describe ">>>>>> \">>=\" and \"do\" <<<<<<" $ do
      it "runWriter multiLogDo shouldBe` (3,[\"Got number: 1\",\"Got number: 2\",\"Got number: 3\"])" $ do
       runWriter multiLogDo `shouldBe` (3,["Got number: 1","Got number: 2","Got number: 3"])      
    describe ">>>>>> \">>=\" and \"do\" <<<<<<" $ do
      it "runWriter multiLogBind `shouldBe` (3,[\"Got number: 1\",\"Got number: 2\",\"Got number: 3\"])" $ do
       runWriter multiLogBind `shouldBe` (3,["Got number: 1","Got number: 2","Got number: 3"])    
    describe ">>>>>> \"greatestCommonDenominator\" <<<<<<" $ do
      it "fst $ runWriter $ greatestCommonDenominator 8 3 `shouldBe` 1" $ do
       (fst $ runWriter $ greatestCommonDenominator 8 3 ) `shouldBe` 1
    describe ">>>>>> \"greatestCommonDenominator\" <<<<<<" $ do
      it "snd $ runWriter $ greatestCommonDenominator 8 3  `shouldBe` [\"8 mod 3 = 2\", \"3 mod 2 = 1\", \"2 mod 1 = 0\", \"Finished with 1\"]" $ do
        ( snd $ runWriter $ greatestCommonDenominator 8 3 )   `shouldBe` ["8 mod 3 = 2", "3 mod 2 = 1", "2 mod 1 = 0", "Finished with 1"]
    
