import           Test.Hspec
import           Typeclasses.Algebraic (Buddy (..), Day (..), List (..),
                                        Person (..), PhoneBook, Point (..),
                                        Shape (..), Tree (..), head',
                                        inPhoneBook, lockerLookup, lockers,
                                        makeBuddy, makePerson, phoneBook,
                                        reverse', surface, tail', treeInsert, treeElem)

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
    describe ">>>>>> List <<<<<<" $ do
      it "head' of  (1 :+ (2 :+ (3 :+ None))) `shouldBe` 1" $ do
        let l1 = 1 :+ 2 :+ 3 :+ None
        head' l1 `shouldBe` 1
      it "tail' of  (1 :+ (2 :+ (3 :+ None))) `shouldBe` (2 :+ (3 :+ None))" $ do
        let l1 = 1 :+ 2 :+ 3 :+ None
        let l2 = 2 :+ 3 :+ None
        tail' l1 `shouldBe` l2
      it
        "reverse' (1 :+ (2 :+ (3 :+ None))) `shouldBe` (3 :+ (2 :+ (1 :+ None)))" $ do
        let l1 = 1 :+ 2 :+ 3 :+ None
        let l2 = 3 :+ 2 :+ 1 :+ None
        reverse' l1 `shouldBe` l2
    describe ">>>>>> Tree <<<<<<" $ do
      it "treeInsert  `shouldBe` Node (Node EmptyTree 1 EmptyTree) 2 (Node EmptyTree 3 EmptyTree)" $ do
        let nums = [3,1,2]
        -- :t foldr 
        -- foldr :: Foldable t => (item -> acc -> acc) -> acc -> t items -> acc
        -- foldr takes 3 args, 
        -- the first one being the function to be applied at each fold (which in the end will return the filled accumulator),
        -- the second being the initial state of the accumulator
        -- and the last argument is the list of items to fold over
        let t = foldr treeInsert EmptyTree nums
        t `shouldBe` Node (Node EmptyTree 1 EmptyTree) 2 (Node EmptyTree 3 EmptyTree)
      it "treeElem 12 t `shouldBe` True" $ do
        let nums = [3,1,2, 12, 34, 1, 62, 7,9, 4]
        let t = foldr treeInsert EmptyTree nums
        treeElem 12 t `shouldBe` True
