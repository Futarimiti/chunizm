{-# LANGUAGE ViewPatterns #-}

module Game.Chunizm.Repl.Commands.Parse where

import           Control.Applicative                   (Alternative (..))
import           Control.Monad.Trans.Reader            (ReaderT, ask)
import           Data.Char                             (isSpace)
import           Data.List                             (dropWhileEnd)
import           Game.Chunizm.Core.Types
import           Game.Chunizm.Repl.Commands.CommandSet (query)
import           Text.ParserCombinators.ReadP          (ReadP, eof, get,
                                                        readP_to_S, satisfy,
                                                        skipMany1, skipSpaces)

-- | Break down a user input, as raw string
-- into (command, args).
decompose :: String -> Maybe (String, Maybe String)
decompose (readP_to_S command . trim -> [(res, "")]) = Just res
decompose _                                          = Nothing

-- | Try to recognise a command.
-- The input string should be well-formatted/trimmed.
parseCommand :: Monad m => ReaderT CommandSet m (String -> Maybe Command)
parseCommand = query <$> ask

-- | Try to parse a line of raw user input at REPL
-- into (command, args).
command :: ReadP (String, Maybe String)
command = onlyCmd <|> cmdArg

cmdArg :: ReadP (String, Maybe String)
cmdArg = do skipSpaces
            cmd <- some notSpace
            skipMany1 space  -- at least 1 space
            arg1 <- notSpace
            args <- many get
            skipSpaces
            eof
            return (cmd, Just (arg1:args))

onlyCmd :: ReadP (String, Maybe String)
onlyCmd = do skipSpaces
             cmd <- some notSpace
             skipSpaces
             eof
             return (cmd, Nothing)

-- | Trim spaces before and after some non-space characters.
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

notSpace :: ReadP Char
notSpace = satisfy (not . isSpace)

space :: ReadP Char
space = satisfy isSpace
