module Game.Chunizm.Config.Override where

import           Game.Chunizm.Config.Override.Dhall (overrideDhall)
import           Game.Chunizm.Core.Types

override :: String  -- user configuration in raw source language
         -> String  -- defaults
         -> IO Config
override = overrideDhall
