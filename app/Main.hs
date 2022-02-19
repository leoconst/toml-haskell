module Main where


import qualified Data.Text.IO as T

import qualified Toml
import qualified PrettyPrint


main :: IO ()
main = do
  source <- T.readFile "app/example.toml"
  let
    tableResult = Toml.decode source
  case tableResult of
    Left _ -> print tableResult
    Right table -> T.putStrLn $ PrettyPrint.table table