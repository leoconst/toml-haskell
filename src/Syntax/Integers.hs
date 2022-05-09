module Syntax.Integers
  ( integer
  ) where


import Control.Applicative (liftA2)
import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Parser


integer =
  oneOf
    [ binary
    , octal
    , hexadecimal
    , decimal
    ]

decimal =
  signed unsignedDecimal

unsignedDecimal =
  zero <|> nonZero

zero =
  0 <$ character '0'

nonZero =
  digitsToIntegerWithBase 10 <$> digits
  where
    digits =
      liftA2 (++) headDigits tailDigits

    headDigits =
      liftA2 (:)
        (digit (characterRange '1' '9'))
        (zeroOrMore decimalDigit)

    tailDigits =
      concat <$>
        zeroOrMore (character '_' *> oneOrMore decimalDigit)

    decimalDigit =
      digit decimalDigitCharacter


signed =
  liftA2 decode optionalSign
  where
    decode sign value =
      case sign of
        Positive -> value
        Negative -> negate value

optionalSign =
  oneOf
    [ Negative <$ character '-'
    , Positive <$ optional (character '+')
    ]

data Sign
  = Positive
  | Negative


binary =
  specialInteger 'b' 2 binaryDigit
  where
    binaryDigit =
      zero <|> one

    one =
      1 <$ character '1'


octal =
  specialInteger 'o' 8 octalDigit

octalDigit =
  digit (characterRange '0' '7')


hexadecimal =
  specialInteger 'x' 16 hexadecimalDigit

hexadecimalDigit =
  digit hexadecimalDigitCharacter

hexadecimalDigitCharacter =
  oneOf
    [ decimalDigitCharacter
    , characterRange 'A' 'F'
    , characterRange 'a' 'f'
    ]


decimalDigitCharacter =
  characterRange '0' '9'

digit characterParser =
  (toInteger . Char.digitToInt)
    <$> characterParser

specialInteger prefix base digit =
  character '0' *> character prefix *>
    (digitsToIntegerWithBase base <$> oneOrMore digit)

digitsToIntegerWithBase base digits =
  sum (zipWith (*) (reverse digits) (map (base^) [0..]))