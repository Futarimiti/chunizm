-- | Show things in a human-readable way.
module Game.Chunizm.Board.Show ( showTheOpened
                               , showTheBoard
                               , showSolutions
                               ) where

import           Control.Monad.Trans.Reader (ReaderT (..), asks, withReaderT)
import           Data.Set                   (Set, toList)
import           Game.Chunizm.Board.Reveal  (getPuzzles, getSolutions)
import           Game.Chunizm.Core.Types
import           Numeric.Natural            (Natural)
import           Text.Printf                (printf)

-- | Represent the board in a human readable way.
-- Using @boardListing@ for formatting.
showTheBoard :: Monad m => ReaderT Global m (Board -> String)
showTheBoard = do getPs <- withReaderT config getPuzzles
                  f getPs

-- | Represent the solutions in a human readable way.
-- Using @boardListing@ for formatting.
showSolutions :: Monad m => ReaderT Global m (Board -> String)
showSolutions = f getSolutions

-- DRY extraction
f :: Monad m
  => (Board -> [String])  -- getPuzzle or getSolutions
  -> ReaderT Global m (Board -> String)
f getSome = do listing <- asks (boardListing . config)
               emptyMsg <- asks (emptyBoard . info)
               return $ \b -> case getSome b of
                                [] -> emptyMsg
                                xs -> unlines $ zipWith (printf listing) ([1..] :: [Natural]) xs

-- | Show the opened characters in a human readable way.
-- The user configuration is unused, though I may add an option some time.
showTheOpened :: Monad m => ReaderT Config m (Set Char -> String)
showTheOpened = return $ ("Opened: " ++) . (\l -> if null l then "[]" else l) . toList
