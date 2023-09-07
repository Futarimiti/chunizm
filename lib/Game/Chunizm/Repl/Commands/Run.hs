-- | Running commands, collecting results,
-- rendering outputs / errors,
-- modify round status, etc.
module Game.Chunizm.Repl.Commands.Run where

import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT (runReaderT))
import           Control.Monad.Trans.State  (StateT (runStateT))
import           Game.Chunizm.Core.Types

-- | Run a REPL command,
-- provided args, global constants and round context.
runCommand :: Command
           -> Maybe String
           -> ReaderT Global (StateT Round (ExceptT Error IO)) Outcome
runCommand = id

-- | Run a REPL command and return the result in @IO@ context.
runIO :: Command -> Maybe String -> Global -> Round -> IO (Either Error (Outcome, Round))
runIO cmd args g r = runExceptT (runStateT (runReaderT (runCommand cmd args) g) r)

