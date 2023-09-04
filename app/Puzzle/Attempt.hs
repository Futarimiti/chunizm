{-# LANGUAGE ViewPatterns #-}

module Puzzle.Attempt where

import           Config.Types              (Config (accentSensitive, caseSensitive))
import           Control.Monad.Trans.State (StateT, get)
import           Data.Char                 (toUpper)
import           Data.Function             (on)
import           Data.Functor.Compose      (Compose (..))
import           Data.List                 (findIndex)
import           Puzzle.Reveal             (revealN, uncensorAll)
import           Puzzle.Types              (Puzzle)
import           Util                      (canon, canonical)

-- | Attempt a guess
-- Will reveal the corresponding entry upon success
attempt :: Monad m => Config -> [String] -> StateT Puzzle m Bool
attempt c xs = do p <- get
                  case findIndex (match xs) (uncensorAll p) of
                    Nothing                  -> return False
                    Just (fromIntegral -> n) -> do revealN n
                                                   return True
                    where match w s = matches c w (words s)

matches :: Config
        -> [String]  -- guess
        -> [String]  -- Solution1 in words
        -> Bool
matches c = case (caseSensitive c, accentSensitive c) of
                     (True, True) -> (==)
                     (True, False) -> (==) `on` fmap toUpper . Compose
                     (False, True) -> (==) `on` map canonical
                     (False, False) -> (==) `on` fmap toUpper . Compose . map canonical

matchChar :: Config
          -> Char
          -> Char
          -> Bool
matchChar c = case (caseSensitive c, accentSensitive c) of
                     (True, True)   -> (==)
                     (True, False)  -> (==) `on` toUpper
                     (False, True)  -> (==) `on` canon
                     (False, False) -> (==) `on` toUpper . canon

