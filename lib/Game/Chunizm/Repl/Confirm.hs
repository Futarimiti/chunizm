{-# LANGUAGE RecordWildCards #-}

module Game.Chunizm.Repl.Confirm where

import           Control.Monad              (unless)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (ReaderT, asks)
import           Data.Char                  (toUpper)
import           Game.Chunizm.Core.Types
import           System.IO                  (hFlush, stdout)

confirm :: MonadIO m => ReaderT Global m Bool
confirm = do doConfirm <- asks (confirmDestructive . config)
             if doConfirm then do Info {..} <- asks info
                                  liftIO $ do putStrLn confirmation
                                              putStr proceeding
                                              hFlush stdout
                                              input <- getChar
                                              putStrLn ""
                                              let proceed = toUpper input == 'Y'
                                              unless proceed (putStrLn cancelConfirm)
                                              return proceed
                          else return True

