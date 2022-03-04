import qualified Data.Text.IO as T
import System.Exit (die)
import System.IO (stdin)

import qualified Toml
import qualified TomlToJson


main :: IO ()
main = do
  source <- T.hGetContents stdin
  case Toml.decode source of
    Left error -> die (show error)
    Right table -> putStr (TomlToJson.table table)