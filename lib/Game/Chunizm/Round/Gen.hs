module Game.Chunizm.Round.Gen where

import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Set                   as S
import           Game.Chunizm.Board.Gen     (genWithAll, quickGen)
import qualified Game.Chunizm.Board.Gen     as B
import           Game.Chunizm.Core.Types

-- | Start a new "empty" round.
emptyRound :: Round
emptyRound = Round {opened=S.empty, board=B.empty}

-- | Start a new round by randomly select
-- @defaultBoardSize@ songs from @dataSource@.
quickNew :: MonadIO m => ReaderT Config m Round
quickNew = do b <- quickGen
              return emptyRound{board=b}

quickNewAll :: MonadIO m => ReaderT Config m Round
quickNewAll = do b <- genWithAll
                 return emptyRound{board=b}


