module Syntax.Ignored
  ( comment
  , ignoredLines
  , ignoredSurround
  , whitespace
  , whitespaced
  ) where


import Parser
import Syntax.Common


ignoredSurround =
  surrounded ignoredLines

ignoredLines =
  zeroOrMore $
    oneOf
      [ ignored newline
      , ignored comment
      , ignored whitespaceCharacter
      ]


comment =
  character '#' *>
    zeroOrMore (characterNotIn disallowedControlCharacters)


whitespaced =
  surrounded whitespace

whitespace =
  zeroOrMore whitespaceCharacter

whitespaceCharacter =
  oneOf
    [ character ' '
    , character '\t'
    ]