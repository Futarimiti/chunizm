{-# LANGUAGE RecordWildCards #-}

module Puzzle.Types where

import           Config.Types         (Config (..))
import           Data.Functor.Compose
import           Data.List.NonEmpty
import           Data.List.NonEmpty   as NE (cycle)
import           Data.Set             (Set, empty)
import           Player.Types

type Solution = [Solution1]

type Solution1 = String

data PuzzleChar = Exposed Char
                | Censored Char
                deriving (Show, Read, Eq)

type Puzzle1 = [PuzzleChar]

type Puzzle = [Puzzle1]

data Round = Round { puzzle :: Puzzle
                   , opened :: Set Char
                   , player :: Maybe (NonEmpty Player)
                   } deriving (Show, Eq)

startup :: Config -> Puzzle -> Round
startup c p = Round { puzzle = p
                    , opened = empty
                    , player = fmap NE.cycle . getCompose . fmap Player . Compose . nonEmpty $ players c
                    }

-- turn to next player
next :: Round
     -> Round
next r@Round{..} = r{ player = do _ :| rest <- player
                                  nonEmpty rest
                    }
