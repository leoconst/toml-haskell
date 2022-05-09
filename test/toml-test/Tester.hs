import qualified Data.Text.IO as T.IO
import System.Exit (die)
import System.IO (stdin)

import qualified Toml
import qualified TomlToJson


main :: IO ()
main = do
  source <- T.IO.hGetContents stdin
  case Toml.decode source of
    Left error -> die (show error)
    Right table -> T.IO.putStr (TomlToJson.table table)