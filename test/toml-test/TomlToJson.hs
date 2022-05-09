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
  tableWithIndentation insertNewLine
  where
    insertNewLine left right =
      left <> "\n" <> right

tableWithIndentation startNewLine toml =
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
      keyValueWithIndentation startNewLine key value ""

keyValueWithIndentation startNewLine key value text =
     textShow key <> ": " <> valueWithIndentation value (indented startNewLine)
  <> text

valueWithIndentation (Toml.TableValue table) startNewLine =
  tableWithIndentation startNewLine table
valueWithIndentation (Toml.Array values) startNewLine =
  arrayWithIndentation values startNewLine
valueWithIndentation (Toml.String text) startNewLine =
  typedValue "string" text startNewLine
valueWithIndentation (Toml.Integer integer) startNewLine =
  typedValue "integer" (textShow integer) startNewLine
valueWithIndentation (Toml.Float float) startNewLine =
  typedValue "float" (textShow float) startNewLine
valueWithIndentation (Toml.Boolean boolean) startNewLine =
  typedValue "bool" (if boolean then "true" else "false") startNewLine

arrayWithIndentation values startNewLine =
    "["
  `startNewLine` indent <> (T.intercalate separator (map indentValue values))
  `startNewLine` "]"
  where
    separator =
      "," `startNewLine` indent
    indentValue value =
      valueWithIndentation value (indented startNewLine)

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
