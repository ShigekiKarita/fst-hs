module WfstSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
-- import Test.QuickCheck () -- hiding ((.&.))

import Lib

spec :: Spec
spec = do
  describe "+ qc" $ do
    prop "i + i = i * 2" $ \i ->
      (i :: Int) + i == i * 2
  describe "minfree" $ do
    it "founds the minimum nat not in a list" $
      funcc 1 2 `shouldBe` 3
  -- describe "compose" $ do
  --   it "composes to transducers" $
      
