module PrettyPrint
  ( table
  ) where


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Toml (Value(TableValue))


table = tableWithIndentation 0

tableWithIndentation indentation map =
  Map.foldrWithKey (keyValueWithIndentation indentation) "" map


keyValueWithIndentation indentation key value text =
     T.replicate indentation "\t"
  <> textShow key <> ": " <> valueWithIndentation value indentation
  <> text


valueWithIndentation (TableValue tableValue) indentation =
  "\n" <> tableWithIndentation (indentation + 1) tableValue
valueWithIndentation value indentation =
  textShow value <> "\n"


textShow showable = T.pack $ show showable