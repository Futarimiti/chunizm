{-# LANGUAGE TypeApplications #-}

module Repl (repl) where

import           Config               (Config (..), userConfig, userPrompt)
import           Control.Exception    (SomeException, catch)
import           Control.Monad        (when, (>=>))
import           Data.Functor         ((<&>))
import qualified Data.Map.Lazy        as M
import           Data.Maybe           (fromMaybe)
import           Error                (aritiesMismatch, arityMismatch,
                                       cmdNotFound, illegalArg)
import           Info                 (endMsg, info)
import           Puzzle               (Puzzle, attempt, emptyPuzzle,
                                       labelledSolutions, printPuzzle, reveal,
                                       selectFromDB, showPuzzle, solutions)
import           Safe                 (atMay)
import           System.IO            (hFlush, stdout)
import           System.Posix.Signals (Handler (Catch), installHandler, sigINT)
import           Text.Printf          (printf)
import           Text.Read            (readMaybe)

repl :: IO ()
repl = installSIGINTHandler >> catch @SomeException (replWithPuzzle emptyPuzzle) (const $ putStrLn "" >> (endMsg <$> info) >>= putStrLn)

replWithPuzzle :: Puzzle -> IO ()
replWithPuzzle p = do putPrompt
                      hFlush stdout
                      input <- getLine
                      res <- runReplCmd p input
                      let output = extractOutput res
                      putOutputLn output
                      let newPuzzle = extractPuzzle res
                      mapM_ printPuzzle newPuzzle
                      let continue = extractContinue res
                      when (isContinue continue) (replWithPuzzle (fromMaybe p newPuzzle))

type Command = Args -> Puzzle -> IO ReplResult

replCmds :: Map [String] Command
replCmds = fromList [ (["uncover", "open"], uncover)
                    , (["try", "attempt"], try)
                    , (["quit", "exit", "finish"], finish)
                    , (["new", "start", "restart"], new)
                    , (["show"], display)
                    , (["solutions", "sol"], showSolutions)
                    ]

display :: Command
display [] p = showPuzzle' [] p
display ["solutions"] p = showSolutions [] p
display ["puzzle"] p = showPuzzle' [] p
display [x] p = return $ maybe (mkResult (illegalArg x) Continue Nothing)
                               (maybe empty minimal . atMay (solutions p) . subtract 1)
                               (readMaybe x)
                                 where empty = mkResult "" Continue Nothing
                                       minimal sol = mkResult sol Continue Nothing
display xs _ = return $ mkResult (arityMismatch 1 (length xs)) Continue Nothing

showPuzzle' :: Command
showPuzzle' _ p = showPuzzle p >>= \str ->
                     return $ mkResult str Continue Nothing

showSolutions :: Command
showSolutions _ p = return $ mkResult (labelledSolutions p) Continue Nothing

-- generate new puzzle
new :: Command
new [] _ = mkResult "" Continue . Just <$> (userConfig >>= selectFromDB . defaultPuzzleSize)
new [n] _ = maybe (return $ mkResult (illegalArg n) Continue Nothing)
                  (selectFromDB >=> (return . mkResult "" Continue . Just))
                  (readMaybe n)
new xs _ = return $ mkResult (aritiesMismatch [0, 1] (length xs)) Continue Nothing

uncover :: Command
uncover [[c]] p = reveal c p <&> mkResult (printf "Uncovering letter %s..." [c]) Continue . Just
uncover [_] _ = return $ mkResult "You may only uncover a single letter!" Continue Nothing
uncover xs _ = return $ mkResult (arityMismatch 1 (length xs)) Continue Nothing

try :: Command
try [] _ = return $ mkResult (arityMismatch 1 (0 :: Int)) Continue Nothing
try xs p = try1 (unwords xs) p  -- dirty, but may need refactor whole lot of things to fix FIXME

try1 :: String -> Puzzle -> IO ReplResult
try1 word p = attempt word p >>= maybe (return $ mkResult "" Continue (Just p)) (return . mkResult "💥" Continue . Just)

finish :: Command
finish [] _ = info >>= (\e ->
                 return $ mkResult e Discontinue Nothing) . endMsg
finish xs _ = return $ mkResult (arityMismatch 0 (length xs)) Continue Nothing

runReplCmd :: Puzzle -> String -> IO ReplResult
runReplCmd p input = case words input of
                       [] -> return $ mkResult "" Continue Nothing
                       cmd:args -> maybe
                                     (return $ mkResult (cmdNotFound cmd) Continue Nothing)
                                     (\f -> f args p)
                                     (replCmds !? cmd)

--- impl

type Output = String
data Continue = Continue | Discontinue
type ReplResult = (Output, Continue, Maybe Puzzle)

mkResult :: Output -> Continue -> Maybe Puzzle -> ReplResult
mkResult = (,,)

type Args = [String]
type Map = M.Map

extractContinue :: ReplResult -> Continue
extractContinue (_, a, _) = a

extractOutput :: ReplResult -> Output
extractOutput (a, _, _) = a

extractPuzzle :: ReplResult -> Maybe Puzzle
extractPuzzle (_, _, p) = p

isContinue :: Continue -> Bool
isContinue Continue    = True
isContinue Discontinue = False

putOutputLn :: Output -> IO ()
putOutputLn "" = return ()
putOutputLn xs = putStrLn xs

putPrompt :: IO ()
putPrompt = userPrompt >>= putStr

fromList :: Ord k => [(k, a)] -> Map k a
fromList = M.fromList

installSIGINTHandler :: IO Handler
installSIGINTHandler = installHandler sigINT (Catch (putStrLn "" >> putPrompt)) Nothing

(!?) :: M.Map [String] a -> String -> Maybe a
(!?) m str = go str (M.toList m)
  where go _ [] = Nothing
        go s ((xs, v):xx)
          | s `elem` xs = Just v
          | otherwise = go s xx
