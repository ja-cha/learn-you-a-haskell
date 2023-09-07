import           Test.Hspec
import           Typeclasses.Custom (CustomEq(..),TrafficLight(..),  Option(..))

main :: IO ()
main =
  hspec $ do
    describe ">>>>>> Custom Eq <<<<<<" $ do
      it "Red === Red `shouldBe` True" $ do
        Red === Red  `shouldBe` True
      it "Red /== Green `shouldBe` True" $ do
        Red /== Green  `shouldBe` True     
    describe ">>>>>> Custom Option <<<<<<" $ do    
      it "Some(1) == Some(1)  `shouldBe` True" $ do
        Some(1) == Some(1)  `shouldBe` True     
      it "fmap (\"Jan \" ++) (Some \"Abt\")  `shouldBe` Some(\"Jan Abt\" )" $ do
        fmap ("Jan " ++) (Some "Abt")  `shouldBe` Some("Jan Abt")     
      it "fmap (+2) (Some 1)  `shouldBe` Some(3)" $ do
        fmap (+2) (Some 1)  `shouldBe` Some(3)     
      it "fmap (+2) (None)  `shouldBe` None" $ do
        fmap (+2)  None  `shouldBe` None     
      it "fmap (1:) (Some [2])  `shouldBe` Some [1,2]" $ do
        fmap (1:) (Some [2])  `shouldBe` Some [1,2]
     