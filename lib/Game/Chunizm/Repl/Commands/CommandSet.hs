module Game.Chunizm.Repl.Commands.CommandSet where

import           Data.Map                ((!?))
import           Game.Chunizm.Core.Types

query :: CommandSet -> String -> Maybe Command
query = (!?)

