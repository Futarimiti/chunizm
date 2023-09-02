module DB (readDB) where

import           Puzzle.Types (Solution)

-- | Fetch all lines of entries in a DB file.
readDB :: FilePath -> IO Solution
readDB = fmap lines . readFile

