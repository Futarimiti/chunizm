module Game.Chunizm (chunizm) where

import           Game.Chunizm.Core.Types
import           Game.Chunizm.Repl       (startRepl)

chunizm :: Global -> IO ()
chunizm = startRepl
