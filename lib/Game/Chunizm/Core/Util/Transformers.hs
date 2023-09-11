{-# LANGUAGE TupleSections #-}

module Game.Chunizm.Core.Util.Transformers where

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.RWS

-- | Lift from `ReaderT` to `RWST`.
-- how could this not be included in the official library?
liftReaderT :: (Functor m, Monoid w) => ReaderT r m a -> RWST r w s m a
liftReaderT (ReaderT f) = RWST $ \r s -> (, s, mempty) <$> f r

