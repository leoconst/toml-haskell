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

    it "decodes top-level keys and a table" $
      Toml.decode "a=1\nb=2\n[c]\nd=3"
        `shouldBe` Right (Map.fromList [
          ("a", Toml.Integer 1),
          ("b", Toml.Integer 2),
          ("c", Toml.TableValue (Map.fromList [("d", Toml.Integer 3)]))
        ])

    it "errors for a duplicate key" $
      Toml.decode "key=1\nkey=2"
        `shouldBe` Left (Toml.DuplicateKey "key")