module Syntax.Ast
  ( Ast(..)
  , Declaration(..)
  , Key(..)
  , Value(..)
  ) where


import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)


data Ast
  = Ast [Declaration]

data Declaration
  = Table Key
  | KeyValue Key Value
  deriving (Show)

data Key
  = Key { parts :: NonEmpty Text }
  deriving (Show)

data Value
  = Integer Integer
  | String Text
  | Boolean Bool
  | Array [Value]
  deriving (Show)