module Syntax.Boolean
  ( boolean
  ) where


import Parser
import Syntax.Common


boolean =
  true <|> false

true = keyword True "true"
false = keyword False "false"