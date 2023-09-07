{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Game.Chunizm.Board.Gen where

import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Trans.Class     (MonadTrans (lift))
import           Control.Monad.Trans.Reader    (ReaderT (..), ask, asks)
import           Data.Char                     (isSpace)
import           Data.Functor.Compose          (Compose (..))
import           Game.Chunizm.Core.Types
import           Game.Chunizm.DataSource.Parse (parseDB)
import           Game.Chunizm.DataSource.Take  (pick, pickAll)
import           Numeric.Natural               (Natural)

-- | Pretty self-explanatory.
empty :: Board
empty = []

-- | Create a board given the solutions.
-- The generated board may or may not expose space characters,
-- depending on the user configuration.
mkBoard :: Config -> [String] -> Board
mkBoard (hideSpace -> True) = getCompose . fmap (\case c | isSpace c -> Exposed c
                                                       c             -> Hidden c) . Compose
mkBoard _ = getCompose . fmap Hidden . Compose

-- | Look into user configuration for @dataSource@
-- and randomly take out @n@ songs, shuffled,
-- to generate a board.
gen :: MonadIO io => Natural -> ReaderT Config io Board
gen size = do c <- ask
              db <- asks dataSource
              songs <- lift $ parseDB db
              selected <- lift $ pick size songs
              return $ mkBoard c selected

-- | Using @defaultBoardSize@ field to generate a board.
quickGen :: MonadIO io => ReaderT Config io Board
quickGen = do size <- asks defaultBoardSize
              gen size

-- | Use all the songs within @dataSource@ to generate a board.
genWithAll :: MonadIO io => ReaderT Config io Board
genWithAll = do c <- ask
                db <- asks dataSource
                songs <- lift $ parseDB db
                shuffled <- pickAll songs
                return $ mkBoard c shuffled
