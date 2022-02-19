module Syntax.Common
  ( disallowedControlCharacters
  , keyword
  , newline
  ) where


import Parser


disallowedControlCharacters =
  [ Range '\x0000' '\x0008'
  , Range '\x000A' '\x001F'
  , Single '\x007F'
  ]

keyword value name =
  value <$ text name

newline =
  string "\r\n" <|> string "\n"