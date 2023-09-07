module Game.Chunizm.DataSource.Parse where

import           Control.Monad             (guard)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           System.Directory          (doesFileExist)

-- | NOTE: will panic if file does not exist
parseDB :: MonadIO io
        => FilePath
        -> io [String]  -- songs
parseDB = fmap lines . liftIO . readFile

-- | The same as @parseDB@ but first check if the file exist.
maybeParseDB :: FilePath
             -> MaybeT IO [String]
maybeParseDB f = do exists <- lift $ doesFileExist f
                    guard exists
                    contents <- lift $ readFile f
                    return $ lines contents

