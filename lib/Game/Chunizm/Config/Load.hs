module Game.Chunizm.Config.Load where

import           Control.Applicative       (Alternative (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           System.Directory          (doesFileExist)

-- | Given a set of search paths,
-- return the first existing file.
loadConfig :: [FilePath]  -- | search paths
           -> MaybeT IO FilePath
loadConfig [] = empty
loadConfig (f:fs) = do exists <- lift $ doesFileExist f
                       if exists then return f else loadConfig fs
