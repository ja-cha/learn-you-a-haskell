module Typeclasses.BinaryTreeFunctorSpec() where


import           Typeclasses.BinaryTreeFunctor (BinaryTree (..), treeInsert)
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe ">>>>>> Binary Tree Functor <<<<<<" $ do
      it "Binary Tree treeInsert" $ do 
        let items = [1,3,2]
        let t = foldr treeInsert Empty items
        t `shouldBe`  Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)
      it "Binary Tree treeMap" $ do 
        let items = [1,3,2]
        let t = foldr treeInsert Empty items
        fmap (*2) t `shouldBe`  Node (Node Empty 2 Empty) 4 (Node Empty 6 Empty) 

