import           Test.Hspec
import Utils (quicksort, largestDivisible, factorial', flip', addThreeNumbers, numChainsWhere, zipWithLambda, twoRowComposition, oddSquareSumComposition, oddSquareSumBindings)

main :: IO ()
main =
  hspec $ do
    describe ">>> Operator Precedence" $ do
      it "(+ 3) 3 shouldBe 3 + 3" $ do (+ 3) 3 `shouldBe` 3 + 3
    describe ">>> Quicksort" $ do
      it "quicksort [3, 1, 7, 5]  should be [1, 3, 5, 7]" $ do
        quicksort [3, 1, 7, 5] `shouldBe` [1, 3, 5, 7]
    describe ">>> Largest Divisible" $ do
      it "99554 `shouldBe` largestDivisible 3829" $ do
        99554 `shouldBe` largestDivisible 3829    
    describe ">>> Factorial" $ do
      it "factorial' 5 shouldBe 120" $ do
        factorial' 5 `shouldBe` 120
    describe ">>> Flip" $ do
      it "3 - 2 `shouldBe` flip' (-) 2 3 " $ do
        3 - 2 `shouldBe` flip' (-) 2 3    
    describe ">>> Lambda" $ do
      it "6 `shouldBe` addThreeNumbers 1 2 3" $ do
        6 `shouldBe` addThreeNumbers 1 2 3
    describe ">>> Long Chains" $ do
      it "66 `shouldBe` numChainsWhere (>15)" $ do
        66 `shouldBe` numChainsWhere (>15)
    describe ">>> Long Chains" $ do
      it "[2.0,5.0,10.0,20.0,25.0] `shouldBe` zipWithLambda (\\l r -> (l * 10) / 5)" $ do
        [2.0,5.0,10.0,20.0,25.0] `shouldBe` zipWithLambda (\l r -> (l * 10) / r)
    describe ">>> Composition 1/2" $ do
      it "[2,4,6,8,10,12,14,16,18,20] `shouldBe` twoRowComposition" $ do
        [2,4,6,8,10,12,14,16,18,20] `shouldBe` twoRowComposition
    describe ">>> Composition 2/2" $ do
      it "oddSquareSum using composition " $ do
       166650 `shouldBe` oddSquareSumComposition
    describe ">>> Bindings" $ do
      it "oddSquareSum using bindings " $ do
       166650 `shouldBe` oddSquareSumBindings   
    