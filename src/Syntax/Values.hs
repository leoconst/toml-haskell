module Syntax.Values
  ( value
  ) where


import Parser (character, optional, oneOf, intercalated, bracketed, (<|>))
import Syntax.Ast
import Syntax.Boolean
import Syntax.Ignored
import Syntax.Integers
import Syntax.Strings


value =
  oneOf
    [ Integer <$> integer
    , String <$> string
    , Boolean <$> boolean
    , Array <$> array
    ]

integer =
  oneOf
    [ binary
    , octal
    , hexadecimal
    , decimal
    ]

string =
  oneOf
    [ multilineLiteralString
    , literalString
    , basicString
    ]

boolean =
  true <|> false

array =
  bracketed (character '[') (character ']') values
  where
    values =
      ignoredSurround (intercalated separator value)
        <* optional separator

    separator =
      ignoredSurround (character ',')