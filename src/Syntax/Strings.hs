module Syntax.Strings
  ( anyString
  , literalString
  , basicString
  ) where


import Control.Monad (replicateM)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)

import Parser
import Syntax.Common


anyString =
  oneOf
    [ multilineLiteralString
    , literalString
    , basicString
    ]

literalString =
  stringParser delimiter literalStringCharacters
  where
    delimiter =
      character '\''

multilineLiteralString =
  delimiter *> optional newline *> contentsAndClose
  where
    delimiter =
      string "'''"

    contentsAndClose =
      T.pack <$>
        (contents <++> close)

    contents =
      interspersed line newline

    line =
      interspersed literalStringCharacters quotes

    quotes =
      (string "''" <|> string "'") <++> nonDelimiter

    nonDelimiter =
      (singleton <$> literalStringCharacter) <|> newline

    singleton item =
      [item]

    interspersed value seperator =
      concat <$>
        value <:> zeroOrMore (seperator <++> value)

    close =
      oneOf (map closer ["''", "'", ""])
      where
        closer endingString =
          string endingString <* delimiter

literalStringCharacters =
  zeroOrMore literalStringCharacter

literalStringCharacter =
  characterNotIn (Single '\'':disallowedControlCharacters)


basicString =
  stringParser delimiter contents
  where
    contents =
      zeroOrMore contentCharacter

    contentCharacter =
      unescapedCharacter <|> escapedCharacter

    unescapedCharacter =
      characterNotIn disallowedCharacters

    disallowedCharacters =
       Single escapeCharacter
      :Single delimiterCharacter
      :disallowedControlCharacters

    escapedCharacter =
      (character escapeCharacter *> escape)

    delimiter =
      character delimiterCharacter

    escapeCharacter = '\\'
    delimiterCharacter = '"'


escape =
  characterEscape <|> unicodeEscape

characterEscape =
  oneOf characterEscapes
  where
    characterEscapes =
      map makeCharacterEscape escapeCharacters

    makeCharacterEscape (c, e) =
      e <$ character c

    escapeCharacters =
      [ ('b', '\x0008')
      , ('t', '\x0009')
      , ('n', '\x000A')
      , ('f', '\x000C')
      , ('r', '\x000D')
      , ('"', '\x0022')
      , ('\\', '\x005C')
      ]

unicodeEscape =
  decode <$> oneOf options
  where
    decode hexadecimalDigits =
      read ("'\\x" <> hexadecimalDigits <> "'") :: Char

    options =
      map unicode prefixAndLengthPairs

    unicode (prefix, length) =
      character prefix *> replicateM length unicodeCharacter

    prefixAndLengthPairs =
      [ ('u', 4)
      , ('U', 8)
      ]

    unicodeCharacter = oneOf
      [ characterRange '0' '9'
      , characterRange 'A' 'F'
      ]


stringParser delimiter contents =
  T.pack <$> surrounded delimiter contents