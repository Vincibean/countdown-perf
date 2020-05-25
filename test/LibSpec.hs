module LibSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lib

spec :: Spec
spec = do
  describe "Lib.choices'" $ do
    it "returns a list of all possible ways of choosing zero or more elements from a list" $ do
      choices [1, 2] `shouldMatchList` [[], [1], [2], [1, 2], [2, 1]]

  describe "Lib.split'" $ do
    it "returns a list of all possible ways of splitting a list into two non-empty parts" $ do
      split [1, 2, 3, 4] `shouldBe` [([1], [2, 3, 4]), ([1, 2], [3, 4]), ([1, 2, 3], [4])]