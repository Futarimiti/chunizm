module Puzzle.Reveal where

import           Control.Monad.Trans.State (StateT (..), modify)
import           Numeric.Natural           (Natural)
import           Puzzle.Types
import           Util                      (updateAt)

reveal :: PuzzleChar -> PuzzleChar
reveal (Censored c) = Exposed c
reveal c            = c

reveal1 :: Puzzle1 -> Puzzle1
reveal1 = map reveal

uncensor :: PuzzleChar -> Char
uncensor (Censored c) = c
uncensor (Exposed c)  = c

uncensor1 :: Puzzle1 -> Solution1
uncensor1 = map uncensor

uncensorAll :: Puzzle -> Solution
uncensorAll = map uncensor1

-- | uncensor Nth entry. (0-based)
revealN :: Monad m => Natural -> StateT Puzzle m ()
revealN n = modify (updateAt n reveal1)
