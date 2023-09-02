{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module Repl.Commands where

import           Config                    (getUserDBPath)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (get, put)
import           Data.Functor.Compose      (Compose (Compose, getCompose))
import           Error                     (arityMismatch,
                                            canOnlyUncover1Letter, illegalArg)
import           Info                      (Info (endMsg), info)
import           Puzzle.Attempt            (attempt, matchChar)
import           Puzzle.Gen                (mask, quickNew, quickNewAll,
                                            randomSolution)
import           Puzzle.Reveal             (uncensorAll)
import           Puzzle.Show               (showSolution)
import           Puzzle.Types              (PuzzleChar (..))
import           Repl.Types                (Command)
import           Safe                      (atMay)
import           Text.Read                 (readMaybe)

showSolutions :: Command
showSolutions c _ = do p <- get
                       let str = showSolution c (uncensorAll p)
                       return (str, True, False)

display :: Command
display _ [] = return ("", True, True)
display c ["solutions"] = showSolutions c []
display c ["puzzle"] = display c []
display _ [readMaybe -> Just n] = do p <- get
                                     return $ case uncensorAll p `atMay` (n - 1) of
                                                Nothing  -> ("", True, False)
                                                Just sol -> (sol, True, False)
display _ [x] = return (illegalArg x, True, False)
display _ xs = return (arityMismatch 1 (length xs), True, False)

new :: Command
new c [] = do sol <- lift $ quickNew c
              put $ mask sol
              return ("", True, True)
new c ["all"] = do sol <- lift $ quickNewAll c
                   put $ mask sol
                   return ("", True, True)
new c [readMaybe -> Just n] = do dbPath <- lift $ getUserDBPath c
                                 sol <- lift (randomSolution dbPath n)
                                 let p = mask sol
                                 put p
                                 return ("", True, True)
new _ [x] = return (illegalArg x, True, False)
new _ xs = return (arityMismatch 1 (length xs), True, False)

finish :: Command
finish _ _ = do e <- lift $ endMsg <$> info
                return (e, False, False)

try :: Command
try _ [] = return (arityMismatch @Int 1 0, True, True)
try c xs = do correct <- attempt c xs
              return $ if correct then ("💥", True, True)
                                  else ("", True, True)

-- | Reveal occurences of a letter.
uncover :: Command
uncover c [[x]] = do p <- get
                     let revealed = getCompose $ fmap f (Compose p)
                     put revealed
                     return ("", True, True)
                       where f (Censored ch) | matchChar c x ch = Exposed x
                             f ch = ch
uncover _ [_]   = return (canOnlyUncover1Letter, True, True)
uncover _ xs    = return (arityMismatch 1 (length xs), True, False)

