{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module Repl.Stateful where

import           Config                    (putPrompt, userConfig)
import           Config.Types              (Config)
import           Control.Exception         (SomeException, catch)
import           Control.Monad
import           Control.Monad.Trans.State (StateT (runStateT))
import           Error                     (cmdNotFound)
import           Info                      (Info (endMsg), info, putInfoLn)
import           Puzzle.Gen                (empty)
import           Puzzle.Show               (printClipPuzzle)
import           Puzzle.Types              (Puzzle)
import           Repl.Commands             (display, finish, new, showSolutions,
                                            try, uncover)
import           Repl.Types                (Command, Outcome)
import           System.IO                 (hFlush, stdout)
import           System.Posix
import           Util                      (lookup')

repl :: IO ()
repl = do c <- userConfig
          putInfoLn c
          _ <- installSIGINTHandler c
          catch @SomeException (replWithPuzzle c empty)
                                  (const $ putStrLn "" >> endMsg <$> info >>= putStrLn)

replWithPuzzle :: Config -> Puzzle -> IO ()
replWithPuzzle c p = do putPrompt c
                        hFlush stdout
                        input <- getLine
                        ((output, continue, doPrint), res) <- runStateT (runReplCmd c input) p
                        unless (null output) (putStrLn output)
                        when doPrint (printClipPuzzle c res)
                        when continue (replWithPuzzle c res)

runReplCmd :: Config
            -> String  -- user input in repl
            -> StateT Puzzle IO Outcome
runReplCmd c (words -> cmd:args) = case lookup' cmd replCmds of
                                      Just replCmd -> replCmd c args
                                      Nothing -> return (cmdNotFound cmd, True, False)
runReplCmd _ _ {- empty line -} = skip
  where skip = return ("", True, False)

replCmds :: [([String], Command)]
replCmds = [ (["uncover", "open"], uncover)
           , (["try", "attempt"], try)
           , (["quit", "exit", "finish"], finish)
           , (["new", "start", "restart"], new)
           , (["show"], display)
           , (["solutions", "sol"], showSolutions)
           ]

installSIGINTHandler :: Config -> IO Handler
installSIGINTHandler c = installHandler sigINT (Catch (putStrLn "" >> putPrompt c)) Nothing

