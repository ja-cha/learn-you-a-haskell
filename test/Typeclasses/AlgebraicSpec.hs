module Typeclasses.AlgebraicSpec() where

import           Test.Hspec
import           Typeclasses.Algebraic (Buddy (..), Day (..), Person (..),
                                        PhoneBook, Point (..), Shape (..),
                                        inPhoneBook, lockerLookup, lockers,
                                        makeBuddy, makePerson, phoneBook,
                                        surface)

buddysLastName :: Buddy -> String
buddysLastName (Buddy _ ln _ _) = ln

main :: IO ()
main =
  hspec $ do
    describe ">>>>>> Typeclasses <<<<<<" $ do
      it "Rectangle & Point" $ do
        10000.0 `shouldBe` surface (Rectangle (Point 0 0) (Point 100 100))
      it "Buddy Eq" $ do
        let buddy = makeBuddy "Buddy" "Finkelstein" 54 186.5
        let sameBuddy = Buddy "Buddy" "Finkelstein" 54 186.5
        buddy `shouldBe` sameBuddy
      it "Buddy lastName Eq" $ do
        let buddy = makeBuddy "Buddy" "Finkelstein" 54 186.5
        buddysLastName buddy `shouldBe` "Finkelstein"
      it "Person Eq" $ do
        let person = makePerson "John" "Doe" 34 175.3 "917 512 2767"
        let samePerson = Person "John" "Doe" 34 175.3 "917 512 2767"
        person `shouldBe` samePerson
      it "Person lastName Eq" $ do
        let person = makePerson "John" "Doe" 34 175.3 "917 512 2767"
        lastName person `shouldBe` "Doe"
      it
        "Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43, height = 185.0, phone = \"917 512 2767\"}" $ do
        let mikeD = makePerson "Michael" "Diamond" 43 185.0 "917 512 2767"
        let mikeD2 =
              read
                "Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43, height = 185.0, phone = \"917 512 2767\"}"
        mikeD `shouldBe` mikeD2
      it "Day [Monday .. Thursday]" $ do
        [Monday .. Thursday] `shouldBe` [Monday, Tuesday, Wednesday, Thursday]
      it "PhoneBook inPhoneBook \"lucille\" \"205-2928\" " $ do
        inPhoneBook "lucille" "205-2928" phoneBook `shouldBe` True
      it "LockerMap lockerLookup 101 lockers `shouldBe` Right \"JAH3I\"" $ do
        lockerLookup 101 lockers `shouldBe` Right "JAH3I"
