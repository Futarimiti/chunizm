module Game.Chunizm.DataSource.Take where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.List              (genericTake)
import           Numeric.Natural        (Natural)
import           System.Random.Shuffle  (shuffleM)

-- | Randomly pick n elements,
-- or all the elements from a list,
-- depending on its length.
pick :: MonadIO io => Natural -> [a] -> io [a]
pick n l = fmap (genericTake n) (liftIO $ shuffleM l)

pickAll :: MonadIO io => [a] -> io [a]
pickAll = liftIO . shuffleM
