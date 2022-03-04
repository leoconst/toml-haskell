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
  tableWithIndentation 0

tableWithIndentation indentation toml =
  if Map.null toml
    then "{}"
    else "{"
      <> indentLine (indentation + 1) tableWithKeys
      <> indentLine indentation "}"
  where
    tableWithKeys =
      T.intercalate separator keyValues
    separator =
      ",\n" <> indent (indentation + 1)
    keyValues =
      map keyValue (Map.toList toml)
    keyValue (key, value) =
      keyValueWithIndentation indentation key value ""

keyValueWithIndentation indentation key value text =
     textShow key <> ": " <> valueWithIndentation value (indentation + 1)
  <> text

valueWithIndentation (Toml.TableValue table) indentation =
  tableWithIndentation indentation table
valueWithIndentation (Toml.Array values) indentation =
     "["
  <> indentLine (indentation + 1)
    (T.intercalate (",\n" <> (indent (indentation + 1))) (map (\value -> valueWithIndentation value (indentation + 1)) values))
  <> indentLine indentation "]"
valueWithIndentation (Toml.String text) indentation =
  typedValue "string" text indentation
valueWithIndentation (Toml.Integer integer) indentation =
  typedValue "integer" (textShow integer) indentation
valueWithIndentation (Toml.Float float) indentation =
  typedValue "float" (textShow float) indentation
valueWithIndentation (Toml.Boolean boolean) indentation =
  typedValue "bool" (if boolean then "true" else "false") indentation

typedValue typeName valueString indentation =
     "{"
  <> indentLine (indentation + 1) ("\"type\": \"" <> typeName <> "\",")
  <> indentLine (indentation + 1) ("\"value\": \"" <> valueString <> "\"")
  <> indentLine indentation "}"

indent indentation =
  T.replicate indentation "  "

indentLine indentation line =
  "\n" <> indent indentation <> line

textShow showable = T.pack $ show showable