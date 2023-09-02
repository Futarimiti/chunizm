module Puzzle.Types where

type Solution = [Solution1]

type Solution1 = String

data PuzzleChar = Exposed Char
                | Censored Char
                deriving (Show, Read, Eq)

type Puzzle1 = [PuzzleChar]

type Puzzle = [Puzzle1]

