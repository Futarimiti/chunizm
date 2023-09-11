{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Game.Chunizm.Repl.Commands.Collection ( new
                                             , try
                                             , uncover
                                             , exit
                                             , restart
                                             , display
                                             ) where

import           Control.Monad                       (when)
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Except          (throwE)
import           Control.Monad.Trans.Reader          (ReaderT (..), withReaderT)
import           Control.Monad.Trans.RWS             (RWST (..), asks, gets,
                                                      modify, put, withRWST)
import           Control.Monad.Trans.State           (evalStateT)
import qualified Data.Set                            as Set
import           Game.Chunizm.Board.Gen              (gen)
import qualified Game.Chunizm.Board.Gen              as Board
import           Game.Chunizm.Board.Reveal           (getNthSolution, open)
import           Game.Chunizm.Board.Show             (showSolutions,
                                                      showTheOpened)
import           Game.Chunizm.Board.Try              (attempt)
import           Game.Chunizm.Core.Types
import           Game.Chunizm.Core.Util.Transformers (liftReaderT)
import           Game.Chunizm.Repl.Confirm           (confirm)
import           Game.Chunizm.Round.Gen              (emptyRound, quickNew,
                                                      quickNewAll)
import           Numeric.Natural                     (Natural)
import           Text.Read                           (readMaybe)

-- | Start a new round.
-- @new@: randomly select @defaultBoardSize@ songs from @dataSource@.
-- @new all@: select all songs from @dataSource@, shuffled.
-- @new <nat>@: randomly select @<nat>@ songs from @dataSource@.
-- otherwise, flagged as error.
new :: Command
new Nothing = do r <- liftReaderT $ withReaderT config quickNew
                 put r
                 return Outcome {showBoard=True, output=Nothing, continue=True}
new (Just "all") = do r <- liftReaderT $ withReaderT config quickNewAll
                      put r
                      return Outcome {showBoard=True, output=Nothing, continue=True}
new (Just (readMaybe -> Just n)) = do b <- liftReaderT $ withReaderT config (gen n)
                                      put emptyRound{board=b}
                                      return Outcome {showBoard=True, output=Nothing, continue=True}
new (Just x) = do iae <- asks (illegalArgsError . errors)
                  lift $ throwE (iae x)


-- | Attempt a guess.
try :: Command
try (Just guess) = do c <- asks config
                      b <- gets board
                      correct <- evalStateT (runReaderT (attempt guess) c) b
                      return Outcome {showBoard=True, output=if correct then Just "💥" else Nothing, continue=True}
try Nothing = do ame <- asks (arityMismatchError . errors)
                 lift $ throwE (ame 1 0)

-- | Uncover a letter.
uncover :: Command
uncover (Just [c]) = do withRWST ((,) . config) $ open c
                        return Outcome {showBoard=True, output=Nothing, continue=True}
uncover (Just x) = do iae <- asks (illegalArgsError . errors)
                      lift $ throwE (iae x)
uncover Nothing = do ame <- asks (arityMismatchError . errors)
                     lift $ throwE (ame 1 0)

-- | Leave REPL.
exit :: Command
exit = const $ do Info{..} <- asks info
                  return Outcome { showBoard = False
                                 , output = Just leaveMessage
                                 , continue = False
                                 }

-- | Restarting REPL, discarding the current board and opened letters.
-- This is a destructive, hence will ask for confirmation first.
restart :: Command
restart = const $ do proceed <- liftReaderT confirm
                     when proceed (modify $ \r -> r {opened=Set.empty, board=Board.empty})
                     return Outcome { showBoard = False
                                    , output = Nothing
                                    , continue = True
                                    }

-- | Display things. Does not modify the round.
-- @display@: show current board
-- @display solutions@: show solutions
-- @display board@: = @display@
-- @display opened@: show opened characters
-- @display <nat>@: show @<nat>@th solution (1-based)
-- otherwise, flagged as error.
display :: Command
display Nothing = displayBoard
display (Just x) = case x of
                     "solutions"           -> displaySolution
                     "board"               -> displayBoard
                     "opened"              -> displayOpened
                     (readMaybe -> Just n) -> displayNth n
                     _                     -> do iae <- asks (illegalArgsError . errors)
                                                 lift $ throwE (iae x)

displayNth :: (Monoid w, Monad m) => Natural -> RWST Global w Round m Outcome
displayNth n = do b <- gets board
                  return Outcome {showBoard=False, output=getNthSolution n b  -- if index exceeds list length, print nothing
                    , continue=True}

-- displayOpened :: ReaderT Global (StateT Round (ExceptT String IO)) Outcome
displayOpened :: (Monoid w, Monad m) => RWST Global w Round m Outcome
displayOpened = do showO <- liftReaderT $ withReaderT config showTheOpened
                   o <- gets opened
                   return Outcome {showBoard=False, output=Just (showO o), continue=True}

displaySolution :: (Monoid w, Monad m) => RWST Global w Round m Outcome
displaySolution = do showS <- liftReaderT showSolutions
                     b <- gets board
                     return Outcome {showBoard=False, output=Just (showS b), continue=True}

displayBoard :: (Monoid w, Monad m) => RWST Global w Round m Outcome
displayBoard = return Outcome {showBoard=True, output=Nothing, continue=True}
