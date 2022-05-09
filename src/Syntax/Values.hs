module Syntax.Values
  ( value
  ) where


import Parser
import Syntax.Ast
import Syntax.Boolean
import Syntax.Ignored
import Syntax.Integers
import Syntax.Strings


value =
  oneOf
    [ Integer <$> integer
    , String <$> anyString
    , Boolean <$> boolean
    , Array <$> array
    ]

array =
  bracketed (character '[') (character ']') values
  where
    values =
      ignoredSurround (intercalated separator value)
        <* optional separator

    separator =
      ignoredSurround (character ',')