import Test.Hspec
import qualified Toml

import Data.Either (isRight)
import qualified Data.Map as Map


main :: IO ()
main = hspec $ do
  describe "Toml.decode" $ do
    it "returns empty table for empty source" $
      Toml.decode "" `shouldBe` Right Map.empty