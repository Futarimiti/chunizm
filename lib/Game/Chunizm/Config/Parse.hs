module Game.Chunizm.Config.Parse where

import           Data.Text                       (Text)
import           Game.Chunizm.Config.Parse.Dhall
import           Game.Chunizm.Core.Types

parseConfig :: Text -> IO Config
parseConfig = parseDhall

parseFile :: FilePath -> IO Config
parseFile = parseDhallFile
