module Repl.Types where

import           Config.Types              (Config)
import           Control.Monad.Trans.State
import           Puzzle.Types

type Outcome = ( String  -- output
               , Bool    -- True: continue; False: terminate
               , Bool    -- print current puzzle?
               )

type Command = Config
            -> [String]  -- arguments
            -> StateT Puzzle IO Outcome

