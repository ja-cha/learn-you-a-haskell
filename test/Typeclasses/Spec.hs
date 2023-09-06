import           Test.Hspec
import           Typeclasses.Algebraic (Buddy (..), Day (..), List (..),
                                        Person (..), PhoneBook, Point (..),
                                        Shape (..), head', inPhoneBook,
                                        lockerLookup, lockers, makeBuddy,
                                        makePerson, phoneBook, reverse',
                                        surface, tail')

buddysLastName :: Buddy -> String
buddysLastName (Buddy _ ln _ _) = ln

main :: IO ()
main =
  hspec $ do
    describe ">>> Typeclasses" $ do
      it "Data type: Rectangle & Point" $ do
        10000.0 `shouldBe` surface (Rectangle (Point 0 0) (Point 100 100))
      it "Data type: Buddy Eq" $ do
        let buddy = makeBuddy "Buddy" "Finkelstein" 54 186.5
        let sameBuddy = Buddy "Buddy" "Finkelstein" 54 186.5
        buddy `shouldBe` sameBuddy
      it "Data type: Buddy lastName Eq" $ do
        let buddy = makeBuddy "Buddy" "Finkelstein" 54 186.5
        buddysLastName buddy `shouldBe` "Finkelstein"
      it "Data type: Person Eq" $ do
        let person = makePerson "John" "Doe" 34 175.3 "917 512 2767"
        let samePerson = Person "John" "Doe" 34 175.3 "917 512 2767"
        person `shouldBe` samePerson
      it "Data type: Person lastName Eq" $ do
        let person = makePerson "John" "Doe" 34 175.3 "917 512 2767"
        lastName person `shouldBe` "Doe"
      it
        "show Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43, height = 185.0, phone = \"917 512 2767\"}" $ do
        let mikeD = makePerson "Michael" "Diamond" 43 185.0 "917 512 2767"
        let mikeD2 =
              read
                "Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43, height = 185.0, phone = \"917 512 2767\"}"
        mikeD `shouldBe` mikeD2
      it "[Monday .. Thursday]" $ do
        [Monday .. Thursday] `shouldBe` [Monday, Tuesday, Wednesday, Thursday]
      it "inPhoneBook \"lucille\" \"205-2928\" " $ do
        inPhoneBook "lucille" "205-2928" phoneBook `shouldBe` True
      it "lockerLookup 101 lockers `shouldBe` Right \"JAH3I\"" $ do
        lockerLookup 101 lockers `shouldBe` Right "JAH3I"
      it "head' of  (1 :+ (2 :+ (3 :+ None))) `shouldBe` 1" $ do
        let l1 = 1 :+ 2 :+ 3 :+ None
        head' l1 `shouldBe` 1
      it "tail' of  (1 :+ (2 :+ (3 :+ None))) `shouldBe` (2 :+ (3 :+ None))" $ do
        let l1 = 1 :+ 2 :+ 3 :+ None
        let l2 = 2 :+ 3 :+ None
        tail' l1 `shouldBe` l2
      it "reverse' (1 :+ (2 :+ (3 :+ None))) `shouldBe` (3 :+ (2 :+ (1 :+ None)))" $ do
        let l1 = 1 :+ 2 :+ 3 :+ None
        let l2 = 3 :+ 2 :+ 1 :+ None
        reverse' l1 `shouldBe` l2
        