import qualified Data.Map as Map

import qualified Toml

import Test.Hspec


main :: IO ()
main = hspec $ do

  describe "Toml.decode" $ do

    it "returns empty table for empty source" $
      Toml.decode ""
        `shouldBe` Right Map.empty

    it "decodes a simple key-value pair" $
      Toml.decode "key = 'value'"
        `shouldBe` Right (Map.fromList [("key", Toml.String "value")])

    it "decodes a single-quoted key" $
      Toml.decode "'Quoted key' = 123"
        `shouldBe` Right (Map.fromList [("Quoted key", Toml.Integer 123)])

    it "decodes a double-quoted key" $
      Toml.decode "\"Quoted key\" = 123"
        `shouldBe` Right (Map.fromList [("Quoted key", Toml.Integer 123)])