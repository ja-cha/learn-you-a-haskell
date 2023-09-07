import           Test.Hspec
import           Typeclasses.RecursiveDS (List (..), Tree (..), head',
                                              reverse', tail', treeElem,
                                              treeInsert)

main :: IO ()
main =
  hspec $ do
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
      it
        "treeInsert  `shouldBe` Node (Node EmptyTree 1 EmptyTree) 2 (Node EmptyTree 3 EmptyTree)" $ do
        let nums = [3, 1, 2]
        -- :t foldr
        -- foldr :: Foldable t => (item -> acc -> acc) -> acc -> t items -> acc
        -- foldr takes 3 args,
        -- the first one being the function to be applied at each fold (which in the end will return the filled accumulator),
        -- the second being the initial state of the accumulator
        -- and the last argument is the list of items to fold over
        let t = foldr treeInsert EmptyTree nums
        t `shouldBe`
          Node (Node EmptyTree 1 EmptyTree) 2 (Node EmptyTree 3 EmptyTree)
      it "treeElem 12 t `shouldBe` True" $ do
        let nums = [3, 1, 2, 12, 34, 1, 62, 7, 9, 4]
        let t = foldr treeInsert EmptyTree nums
        treeElem 12 t `shouldBe` True
