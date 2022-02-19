{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Parser
  ( Parser
  , Result
  , parse
  , character
  , anyCharacter
  , characterRange
  , characterNotIn
  , Characters(..)
  , text
  , string
  , zeroOrMore
  , oneOrMore
  , nonEmpty
  , intercalated
  , intercalatedNonEmpty
  , (<|>)
  , (<:>)
  , (<++>)
  , oneOf
  , optional
  , ignored
  , surrounded
  , bracketed
  ) where


import Control.Applicative (liftA2, Alternative, empty, (<|>), some, many)
import Control.Monad (MonadPlus)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import Data.Text (Text)
import Numeric.Natural (Natural)


type Result a = Either Text a

newtype Parser a =
  Parser { run :: Text -> Result (a, Text) }

instance Functor Parser where
  fmap mapper parser =
    pure mapper <*> parser

instance Applicative Parser where
  pure value =
    Parser $ \source ->
      return (value, source)
  left <*> right =
    Parser $ \source -> do
      (function, leftTail) <- run left source
      (argument, rightTail) <- run right leftTail
      return (function argument, rightTail)

instance Monad Parser where
  parser >>= function =
    Parser $ \source -> do
      (result, tail) <- run parser source
      run (function result) tail

instance Alternative Parser where
  empty =
    Parser $ \source ->
      failed "Empty parser never matches"
  left <|> right =
    Parser $ \source ->
      case run left source of
        Left _ -> run right source
        success -> success

instance MonadPlus Parser


failed = Left


parse :: Parser a -> Text -> Result a
parse parser source = do
  (result, tail) <- run parser source
  if T.null tail
    then return result
    else failed ("Incomplete parse, tail: " <> tail)


character :: Char -> Parser Char
character char =
  characterParser (== char) (describeCharacter char)

anyCharacter :: Parser Char
anyCharacter =
  characterParser (const True) "any character"

characterRange :: Char -> Char -> Parser Char
characterRange lower upper =
  characterParser
    (characterIn lower upper)
    ("a character between "
      <> describeCharacter lower
      <> " and "
      <> describeCharacter upper)

characterParser :: (Char -> Bool) -> Text -> Parser Char
characterParser predicate expectation = Parser $ \source ->
  case T.uncons source of
    Just split@(head, _)
      | predicate head -> return split
      | otherwise -> failure (describeCharacter head)
    _ ->
      failure "end of input"
  where
    failure actual =
      failed ("Expected " <> expectation <> ", got " <> actual)


type CharacterSet = [Characters]

data Characters
  = Single Char
  | Range Char Char

characterNotIn :: CharacterSet -> Parser Char
characterNotIn characterSet =
  characterParser
    (notInCharacterSet characterSet)
    (describeNotCharacterSet characterSet)

notInCharacterSet :: CharacterSet -> Char -> Bool
notInCharacterSet characterSet char =
  not (any containsChar characterSet)
  where
    containsChar = \case
      Single single ->
        char == single
      Range lower upper ->
        characterIn lower upper char

characterIn :: Char -> Char -> Char -> Bool
characterIn lower upper char =
  char >= lower && char <= upper

describeNotCharacterSet :: CharacterSet -> Text
describeNotCharacterSet characterSet =
  "a character not in [" <> description <> "]"
  where
    description =
      T.concat (map describeCharacters characterSet)
    describeCharacters = \case
      Single single ->
        T.singleton single
      Range lower upper ->
        T.pack [lower, '-', upper]

describeCharacter :: Char -> Text
describeCharacter char =
  T.pack (show char)


text :: Text -> Parser Text
text value =
  Parser $ \source -> do
    case T.stripPrefix value source of
      Just tail -> return (value, tail)
      Nothing -> failed ("Expected " <> value)

string :: String -> Parser String
string value =
  T.unpack <$> text (T.pack value)


zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

oneOrMore :: Parser a -> Parser [a]
oneOrMore = some

nonEmpty :: Parser a -> Parser (NonEmpty a)
nonEmpty parser =
  nonEmptyParser parser (zeroOrMore parser)

intercalated :: Parser a -> Parser b -> Parser [b]
intercalated inner outer =
  decode <$> optional (intercalatedNonEmpty inner outer)
  where
    decode = \case
      Just b -> NonEmpty.toList b
      Nothing -> []

intercalatedNonEmpty :: Parser s -> Parser a -> Parser (NonEmpty a)
intercalatedNonEmpty inner outer =
  nonEmptyParser outer (zeroOrMore (inner *> outer))

nonEmptyParser :: Parser a -> Parser [a] -> Parser (NonEmpty a)
nonEmptyParser =
  liftA2 (:|)

(<:>) :: Parser a -> Parser [a] -> Parser [a]
(<:>) = liftA2 (:)

(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
(<++>) = liftA2 (++)


oneOf :: [Parser a] -> Parser a
oneOf = foldl1 (<|>)


optional :: Parser a -> Parser (Maybe a)
optional parser = Parser $ \source ->
  return (case run parser source of
    Right (result, tail) -> (Just result, tail)
    Left _ -> (Nothing, source))

ignored :: Parser a -> Parser ()
ignored parser =
  () <$ parser


surrounded :: Parser a -> Parser b -> Parser b
surrounded surrounder =
  bracketed surrounder surrounder

bracketed :: Parser a -> Parser b -> Parser c -> Parser c
bracketed left right parser =
  left *> parser <* right