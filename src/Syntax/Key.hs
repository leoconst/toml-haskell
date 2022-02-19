module Syntax.Key
  ( key
  ) where


import qualified Data.Text as Text

import Parser
import Syntax.Ast
import Syntax.Ignored
import Syntax.Strings


key =
  Key <$>
    intercalatedNonEmpty (whitespaced (character '.')) keyPart

keyPart =
  oneOf
    [ bareKey
    , literalString
    , basicString
    ]

bareKey =
  Text.pack <$>
    oneOrMore bareKeyCharacter

bareKeyCharacter =
  oneOf
    [ characterRange 'A' 'Z'
    , characterRange 'a' 'z'
    , characterRange '0' '9'
    , character '_'
    , character '-'
    ]