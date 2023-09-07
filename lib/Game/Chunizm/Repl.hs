{-# LANGUAGE TypeApplications #-}

module Game.Chunizm.Repl where

import           Control.Exception
import           Control.Monad                    (when)
import           Control.Monad.Trans.Maybe        (MaybeT (runMaybeT),
                                                   hoistMaybe)
import           Control.Monad.Trans.Reader       (ReaderT (runReaderT),
                                                   runReader)
import           Data.Char                        (isSpace)
import           Data.Version                     (makeVersion, showVersion)
import           Game.Chunizm.Core.Types
import           Game.Chunizm.Errors.Show         (renderError)
import           Game.Chunizm.Repl.Commands.Parse (decompose, parseCommand)
import           Game.Chunizm.Repl.Commands.Run   (runIO)
import           Game.Chunizm.Repl.Interrupt      (installSIGINTHandler)
import           Game.Chunizm.Repl.PrintClip      (clipComps, printComps)
import           Game.Chunizm.Round.Gen           (emptyRound)
import           System.IO                        (hFlush, stdout)
import           System.IO.Error

startRepl :: Global -> IO ()
startRepl g = do let c = config g
                 let i = info g
                 putStrLn (welcomeMessage i)
                 when (showVersionAtStart c) $ putStrLn (versionDisplay i . showVersion . makeVersion . map fromIntegral $ version i)
                 when (showDBAtStart c) $ putStrLn (dbDisplay i (dataSource c))
                 hFlush stdout
                 _ <- runReaderT installSIGINTHandler c
                 catch @IOError
                       (repl g emptyRound)
                       (\e -> if isEOFError e then putStrLn "" >> putStrLn (leaveMessage i)
                                              else putStrLn "" >> putStrLn (errorMessage i))

-- |
-- * Put prompt
-- * Get input
-- * Parse as command
--   * Failed -> unless empty (throw error), restart
-- * Exec command
--   * Error arisen -> throw it, restart
-- * Get results
-- * Print them
-- * Clip them
-- * Exit if the results say so
-- * Start new session
repl :: Global -> Round -> IO ()
repl g r = do let c = config g
              let e = errors g

              putStr (replPrompt c)
              hFlush stdout
              raw <- getLine
              cmdargs <- runMaybeT $ do (cmdstr, args) <- hoistMaybe $ decompose raw
                                        cmd <- hoistMaybe $ runReader parseCommand (cmdset g) cmdstr
                                        return (cmd, args)
              case cmdargs of
                Nothing | all isSpace raw -> repl g r
                Nothing -> do runReader renderError c (malformedCmdError e)
                              repl g r
                Just (cmd, args) -> do ei <- runIO cmd args g r
                                       case ei of
                                         Left err -> do runReader renderError c err
                                                        repl g r
                                         Right (o, newR) -> do runReader printComps (g, o, newR)
                                                               runReader clipComps (g, o, newR)
                                                               when (continue o) $ repl g newR

