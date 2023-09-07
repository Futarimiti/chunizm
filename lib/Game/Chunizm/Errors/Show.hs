module Game.Chunizm.Errors.Show where

import           Control.Monad              (unless)
import           Control.Monad.Trans.Reader (ReaderT (..), reader)
import           Game.Chunizm.Core.Types

-- | Show an error message in a human-readable way.
showError :: Error -> String
showError = ("*** Error when executing the last command:\n\t" ++)

-- | Render an error and print it,
-- unless suppressed by the user configuration.
renderError :: Monad m => ReaderT Config m (Error -> IO ())
renderError = reader $ \c e -> do let suppress = suppressErrors c
                                  unless suppress (putStrLn . showError $ e)
