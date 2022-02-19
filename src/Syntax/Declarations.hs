module Syntax.Declarations
  ( toml
  ) where


import Parser
import Syntax.Ast
import Syntax.Common
import Syntax.Ignored
import Syntax.Key
import Syntax.Values


toml =
  Ast <$>
    ignoredSurround (intercalated (newline <* ignoredLines) declaration)

declaration =
  oneOf
    [ table
    , keyValue
    ]

table =
  Table <$>
    bracketed (character '[') (character ']') (whitespaced key)

keyValue =
  KeyValue
    <$> key
    <* whitespaced (character '=')
    <*> value