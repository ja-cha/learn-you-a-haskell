import           Test.Hspec
import           Utils      (addThreeNumbers, factorial', flip',
                             largestDivisible, numChainsWhere,
                             oddSquareSumBindings, oddSquareSumComposition,
                             quicksort, reversePolishNotation,
                             twoRowComposition, zipWithLambda)

main :: IO ()
main =
  hspec $ do
    describe "Operator Precedence" $
      it "(+ 3) 3 shouldBe 3 + 3" $ (+ 3) 3 `shouldBe` 3 + 3
    describe "Quicksort" $
      it "quicksort [3, 1, 7, 5] should be [1, 3, 5, 7]" $
      quicksort [3, 1, 7, 5] `shouldBe` [1, 3, 5, 7]
    describe "Largest Divisible" $
      it "99554 should be largestDivisible 3829" $
      99554 `shouldBe` largestDivisible 3829
    describe "Factorial" $
      it "factorial' 5 shouldBe 120" $ factorial' 5 `shouldBe` 120
    describe "Flip" $
      it "3 - 2 shouldBe flip' (-) 2 3" $ 3 - 2 `shouldBe` flip' (-) 2 3
    describe "Lambda" $
      it "6 shouldBe addThreeNumbers 1 2 3" $ 6 `shouldBe` addThreeNumbers 1 2 3
    describe "Long Chains" $
      it "66 shouldBe numChainsWhere (>15)" $
      66 `shouldBe` numChainsWhere (> 15)
    describe "Lambda" $
      it
        "[2.0,5.0,10.0,20.0,25.0] shouldBe zipWithLambda (\\l r -> (l * 10) / r)" $
      [2.0, 5.0, 10.0, 20.0, 25.0] `shouldBe`
      zipWithLambda (\l r -> (l * 10) / r)
    describe "Composition 1/2" $
      it "[2,4,6,8,10,12,14,16,18,20] shouldBe twoRowComposition" $
      [2, 4, 6, 8, 10, 12, 14, 16, 18, 20] `shouldBe` twoRowComposition
    describe "Composition 2/2" $
      it "oddSquareSum using composition " $
      166650 `shouldBe` oddSquareSumComposition
    describe "Bindings" $
      it "oddSquareSum using bindings " $ 166650 `shouldBe` oddSquareSumBindings
    describe "Reverse Polish notation" $
      it "4037 = 90 34 12 33 55 66 + * - + - " $
      4037 `shouldBe` reversePolishNotation "90 34 12 33 55 66 + * - + -"
    describe "Reverse Polish notation 2" $
      it "15 = 1 2 + 5 *" $
      15 `shouldBe` reversePolishNotation "1 2 + 5 *"
