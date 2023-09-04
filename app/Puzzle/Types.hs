module Puzzle.Types where

import           Data.Set     (Set)
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
                   , next   :: Maybe Player
                   }
