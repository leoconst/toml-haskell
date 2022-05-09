module TomlToJson
  ( table
  ) where


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import qualified Toml


main =
  T.IO.putStrLn $ table (Map.fromList [
    ("a", Toml.Integer 3),
    ("empty", Toml.TableValue Map.empty),
    ("t", Toml.TableValue (Map.fromList [
      ("b", Toml.Boolean True),
      ("s", Toml.Array [
        Toml.Integer 1,
        Toml.Float 1.5,
        Toml.Boolean False,
        Toml.String "What's going on?"])]))
    ])


table =
  tableWithNewLine "\n"

tableWithNewLine newLine toml =
  if Map.null toml
    then "{}"
    else "{"
      <> newLine <> indent <> tableWithKeys
      <> newLine <> "}"
  where
    tableWithKeys =
      T.intercalate separator keyValues
    separator =
      "," <> newLine <> indent
    keyValues =
      map keyValue (Map.toList toml)
    keyValue (key, value) =
      keyValueWithNewLine newLine key value ""

keyValueWithNewLine newLine key value text =
     textShow key <> ": " <> valueWithNewLine value (newLine <> indent)
  <> text

valueWithNewLine (Toml.TableValue table) newLine =
  tableWithNewLine newLine table
valueWithNewLine (Toml.Array values) newLine =
  arrayWithNewLine values newLine
valueWithNewLine (Toml.String text) newLine =
  typedValue "string" text newLine
valueWithNewLine (Toml.Integer integer) newLine =
  typedValue "integer" (textShow integer) newLine
valueWithNewLine (Toml.Float float) newLine =
  typedValue "float" (textShow float) newLine
valueWithNewLine (Toml.Boolean boolean) newLine =
  typedValue "bool" (if boolean then "true" else "false") newLine

arrayWithNewLine values newLine =
     "["
  <> newLine <> indent <> (T.intercalate separator (map indentValue values))
  <> newLine <> "]"
  where
    separator =
      "," <> newLine <> indent
    indentValue value =
      valueWithNewLine value (newLine <> indent)

typedValue typeName valueString newLine =
     "{"
  <> newLine <> indent <> "\"type\": \"" <> typeName <> "\","
  <> newLine <> indent <> "\"value\": \"" <> valueString <> "\""
  <> newLine <> "}"

indent =
  "  "

textShow showable = T.pack $ show showable
