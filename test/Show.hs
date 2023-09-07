{-# LANGUAGE TypeApplications #-}

module Show where

import           Control.Monad.Trans.Reader          (ReaderT (runReaderT))
import           Control.Monad.Trans.State           (execStateT)
import qualified Data.Set                            as Set
import           Game.Chunizm.Board.Gen
import           Game.Chunizm.Board.Reveal           (open)
import           Game.Chunizm.Board.Show             (showTheBoard)
import           Game.Chunizm.Config.Defaults.Values (defaults)
import           Game.Chunizm.Core.Types             (Round (..))

showExampleBoard :: IO ()
showExampleBoard = do showIt <- runReaderT showTheBoard defaults
                      let b = mkBoard defaults ["Wah!", "MassacrE || 3rc4224M"]
                      openedRound <- execStateT @IO (runReaderT (open 's') defaults) (Round { board = b, opened = Set.empty })
                      let openedBoard = board openedRound
                      putStr "Board before: "
                      print b
                      putStrLn $ showIt b
                      putStr "Board after: "
                      print openedBoard
                      putStrLn $ showIt openedBoard
