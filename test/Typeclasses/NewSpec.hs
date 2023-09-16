module Typeclasses.NewSpec() where

import           Test.Hspec
import           Typeclasses.New (Pair (..))
import Control.Monad.Writer

main :: IO ()
main =
  hspec $ do
    describe ">>>>>> NewType typeclasses <<<<<<" $ do
      it "fmap (+3) (1,1) `shouldBe` (1,4)" $ do
        let r = fmap (+ 3) (1, 1)
        r `shouldBe` (1, 4)
      it "getPair $ fmap (+3) $  Pair (1,1) `shouldBe` (4,1)" $ do
        let r =
              all
                (== (4, 1))
                [ getPair $ fmap (+ 3) $ Pair (1, 1)
                , getPair (fmap (+ 3) $ Pair (1, 1))
                , getPair (fmap (+ 3) (Pair (1, 1)))
                ]
        r `shouldBe` True
  
