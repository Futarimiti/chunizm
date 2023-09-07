module Game.Chunizm.Config.Override.Dhall (overrideDhall) where

import           Data.Text               (pack)
import           Dhall                   (auto, input)
import           Game.Chunizm.Core.Types (Config)

-- | Dirty and hackish
overrideDhall :: String     -- user config
              -> String     -- default config
              -> IO Config  -- final config
overrideDhall user def = input auto $ pack (def ++ " // " ++ user)

