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
     textShow key <> ": " <> valueWithIndentation value startNewLine
  <> text

valueWithIndentation value startNewLine =
  output value
  where
    output = \case
      Toml.TableValue table ->
        tableWithIndentation indentedStartNewLine table
      Toml.Array values ->
        arrayWithIndentation values indentedStartNewLine
      Toml.String text ->
        typedValue "string" text indentedStartNewLine
      Toml.Integer integer ->
        typedValue "integer" (textShow integer) indentedStartNewLine
      Toml.Float float ->
        typedValue "float" (textShow float) indentedStartNewLine
      Toml.Boolean boolean ->
        typedValue "bool" (if boolean then "true" else "false") indentedStartNewLine
    indentedStartNewLine left right =
      left `startNewLine` indent <> right

arrayWithIndentation values startNewLine =
    "["
  `startNewLine` indent <> (T.intercalate separator (map indentValue values))
  `startNewLine` "]"
  where
    separator =
      "," `startNewLine` indent
    indentValue value =
      valueWithIndentation value startNewLine

typedValue typeName valueString startNewLine =
    "{"
  `startNewLine` indent <> "\"type\": \"" <> typeName <> "\","
  `startNewLine` indent <> "\"value\": \"" <> valueString <> "\""
  `startNewLine` "}"

indent =
  "  "

textShow showable = T.pack $ show showable
