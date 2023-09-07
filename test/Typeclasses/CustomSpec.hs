import           Test.Hspec
import           Typeclasses.Custom (CustomEq(..),TrafficLight(..))

main :: IO ()
main =
  hspec $ do
    describe ">>>>>> Custom Eq <<<<<<" $ do
      it "Red === Red `shouldBe` True" $ do
        Red === Red  `shouldBe` True
      it "Red /== Green `shouldBe` True" $ do
        Red /== Green  `shouldBe` True
     