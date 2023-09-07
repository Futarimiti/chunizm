module Game.Chunizm.Config.Parse.Dhall where

import           Data.Text               (Text)
import           Dhall                   (auto, input, inputFile)
import           Game.Chunizm.Core.Types (Config)

parseDhall :: Text -> IO Config
parseDhall = input auto

parseDhallFile :: FilePath -> IO Config
parseDhallFile = inputFile auto
