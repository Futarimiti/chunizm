module Puzzle.Gen where

import           Config               (getUserDBPath)
import           Config.Types
import           Data.Functor.Compose (Compose (..))
import           DB                   (readDB)
import           Numeric.Natural      (Natural)
import           Puzzle.Types
import           Util

empty :: Puzzle
empty = []

mask :: Solution -> Puzzle
mask = getCompose . fmap Censored . Compose

randomSolution :: FilePath  -- where DB is
               -> Natural   -- Amount
               -> IO Solution
randomSolution f n = readDB f >>= pick n

-- | Generate a solution using all the entries within DB.
useAll :: FilePath  -- DB
       -> IO Solution
useAll = readDB

-- | Randomly select default amount of songs
-- from designated db.
quickNew :: Config -> IO Solution
quickNew c = do dbPath <- getUserDBPath c
                randomSolution dbPath (defaultPuzzleSize c)

quickNewAll :: Config -> IO Solution
quickNewAll c = do dbPath <- getUserDBPath c
                   readDB dbPath
