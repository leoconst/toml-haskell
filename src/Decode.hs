module Decode
  ( Table
  , Value(..)
  , decode
  ) where


import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import qualified Parser
import qualified Syntax


type Table = Map.Map Text Value

data Value =
    String Text
  | Integer Integer
  | Float Double
  | Boolean Bool
  | Array [Value]
  | TableValue Table
  deriving (Eq, Show)


decode :: Text -> Parser.Result Table
decode source = do
  (Syntax.Ast lines) <- Parser.parse Syntax.toml source
  readLines lines Map.empty


readLines [] map =
  pure map
readLines ((Syntax.Table astKey):tail) map = do
  key <- unwrapKey astKey
  (tableMap, newTail) <- readTable tail Map.empty
  newMap <- tryInsert key (TableValue tableMap) map
  readLines newTail newMap
readLines ((Syntax.KeyValue key value):tail) map = do
  newMap <- addKeyValueToMap key value map
  readLines tail newMap


readTable ((Syntax.KeyValue key value):tail) map = do
  newMap <- addKeyValueToMap key value map
  readTable tail newMap
readTable tail map =
  pure (map, tail)


addKeyValueToMap astKey astValue map = do
  key <- unwrapKey astKey
  let
    value = unwrapValue astValue
  tryInsert key value map


unwrapKey (Syntax.Key (key:|[])) = Right key
unwrapKey (Syntax.Key keyParts) = Left $
     "Dotted keys are not currently supported: "
  <> T.intercalate "." (NonEmpty.toList keyParts)


tryInsert key value map =
  let
    (existingValue, newMap) =
      Map.insertLookupWithKey handleKey key value map
  in
  case existingValue of
    Just _ -> Left $ "Duplicate key: " <> key
    Nothing -> Right newMap
  where
    handleKey key newValue oldValue = oldValue


unwrapValue astValue = case astValue of
  Syntax.String text -> String text
  Syntax.Integer integer -> Integer integer
  -- TODO: Support other value types:
  -- Syntax.Float double -> Float double
  Syntax.Boolean boolean -> Boolean boolean
  Syntax.Array list -> Array (map unwrapValue list)