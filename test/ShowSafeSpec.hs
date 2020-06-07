module ShowSafeSpec (spec) where

import ShowSafe
import ShowSafe.Import
import Test.Hspec

spec :: Spec
spec =
  describe "Prelude types" $ do
    it "Bool" $ do
      True `looksLike` "True"
      False `looksLike` "False"
    it "[Bool]" $
      [True, True, False] `looksLike` "[True,True,False]"
  where
    looksLike :: (ShowSafe a) => a -> String -> IO ()
    looksLike x y = showSafeDef x `shouldBe` y
