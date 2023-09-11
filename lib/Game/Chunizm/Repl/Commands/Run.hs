-- | Running commands, collecting results,
-- rendering outputs / errors,
-- modify round status, etc.
module Game.Chunizm.Repl.Commands.Run where

import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.RWS    (RWST (runRWST))
import           Game.Chunizm.Core.Types

-- | Run a REPL command,
-- provided args, global constants and round context.
runCommand :: Command
           -> Maybe String
           -> RWST Global () Round (ExceptT Error IO) Outcome
runCommand = id

-- | Run a REPL command and return the result in @IO@ context.
runIO :: Command -> Maybe String -> Global -> Round -> IO (Either Error (Outcome, Round))
runIO cmd args g r = runExceptT $ (\(o, r_, _) -> (o, r_)) <$> runRWST (runCommand cmd args) g r

