module Game.Chunizm.Repl.Interrupt where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, asks)
import           Game.Chunizm.Core.Types
import           System.IO
import           System.Posix               (Handler (..), installHandler,
                                             sigINT)

installSIGINTHandler :: ReaderT Config IO Handler
installSIGINTHandler = do p <- asks replPrompt
                          lift $ installHandler sigINT (Catch (putStrLn "" >> putStr p >> hFlush stdout)) Nothing

