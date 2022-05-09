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
  tableWithNewLine insertNewLine
  where
    insertNewLine left right =
      left <> "\n" <> right

tableWithNewLine startNewLine toml =
  if Map.null toml
    then "{}"
    else "{"
      `startNewLine` indent <> tableWithKeys
      `startNewLine` "}"
  where
    tableWithKeys =
      T.intercalate separator keyValues
    separator =
      "," `startNewLine` indent
    keyValues =
      map keyValue (Map.toList toml)
    keyValue (key, value) =
      keyValueWithNewLine startNewLine key value ""

keyValueWithNewLine startNewLine key value text =
     textShow key <> ": " <> valueWithNewLine value (indented startNewLine)
  <> text

valueWithNewLine (Toml.TableValue table) startNewLine =
  tableWithNewLine startNewLine table
valueWithNewLine (Toml.Array values) startNewLine =
  arrayWithNewLine values startNewLine
valueWithNewLine (Toml.String text) startNewLine =
  typedValue "string" text startNewLine
valueWithNewLine (Toml.Integer integer) startNewLine =
  typedValue "integer" (textShow integer) startNewLine
valueWithNewLine (Toml.Float float) startNewLine =
  typedValue "float" (textShow float) startNewLine
valueWithNewLine (Toml.Boolean boolean) startNewLine =
  typedValue "bool" (if boolean then "true" else "false") startNewLine

arrayWithNewLine values startNewLine =
    "["
  `startNewLine` indent <> (T.intercalate separator (map indentValue values))
  `startNewLine` "]"
  where
    separator =
      "," `startNewLine` indent
    indentValue value =
      valueWithNewLine value (indented startNewLine)

typedValue typeName valueString startNewLine =
    "{"
  `startNewLine` indent <> "\"type\": \"" <> typeName <> "\","
  `startNewLine` indent <> "\"value\": \"" <> valueString <> "\""
  `startNewLine` "}"

indented insertNewLine left right =
  left `insertNewLine` indent <> right

indent =
  "  "

textShow showable = T.pack $ show showable
