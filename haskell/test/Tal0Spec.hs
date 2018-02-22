module Tal0Spec where

import Test.HUnit (assertFailure)
import Test.Hspec

import Tal0

import qualified Data.Map.Lazy as Map

shouldNotChange :: HasCallStack => Machine -> Expectation
shouldNotChange m1 = case eval1 m1 of
  Right m2 -> m1 `shouldBe` m2
  Left e -> assertFailure $ show e

spec :: Spec
spec = do
  describe "eval1" $
    it "does one-step evaluation" $ do
      let m0 = emptyMachine $ Label 0

      eval1 m0 `shouldBe` Left (NoLabel 0)
      shouldNotChange $ m0 { heap = Map.singleton 0 . Seq [] $ Label 0 }

      let h = Map.singleton 0 . Seq [] $ Label 1
      eval1 m0 { heap = h } `shouldBe` return (emptyMachine $ Label 1) { heap = h }

      let r0 = Register 0
      eval1 m0
        { file = Map.singleton r0 $ Int 13
        , current = Seq [Mov r0 $ Int 100] $ Label 0
        }
        `shouldBe` return m0
        { file = Map.singleton r0 $ Int 100
        , current = Seq [] $ Label 0
        }
      eval1 m0
        { file = Map.singleton r0 $ Int 13
        , current = Seq [Add r0 r0 $ Int 100] $ Label 0
        }
        `shouldBe` return m0
        { file = Map.singleton r0 $ Int 113
        , current = Seq [] $ Label 0
        }
