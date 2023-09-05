import           Test.Hspec
import           Typeclasses.Algebraic (Point (..), Shape (..), surface, Buddy(..), makeBuddy, Person(..), makePerson)

main :: IO ()
main =
  hspec $ do
    describe ">>> Typeclasses" $ do
      it "Data type: Rectangle & Point" $ do
        10000.0 `shouldBe` surface (Rectangle (Point 0 0) (Point 100 100))
      it "Data type: Buddy Eq" $ do
        let buddy = makeBuddy "Buddy"  "Finkelstein" 54 186.5  
        let sameBuddy = Buddy "Buddy"  "Finkelstein" 54 186.5 
        buddy `shouldBe` sameBuddy 
      it "Data type: Buddy lastName Eq" $ do
        let buddy = makeBuddy "Buddy"  "Finkelstein" 54 186.5  
        buddysLastName buddy `shouldBe` "Finkelstein"
      it "Data type: Person Eq" $ do
        let person = makePerson "John"  "Doe" 34 175.3 "917 512 2767"  
        let samePerson = makePerson "John"  "Doe" 34 175.3 "917 512 2767"   
        person `shouldBe` samePerson 
      it "Data type: Person lastName Eq" $ do
        let person = makePerson "John"  "Doe" 34 175.3 "917 512 2767"    
        lastName person  `shouldBe` "Doe"

buddysLastName :: Buddy -> String  
buddysLastName (Buddy _ ln _ _) = ln  
