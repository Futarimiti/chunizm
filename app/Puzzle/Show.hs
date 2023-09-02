module Puzzle.Show ( showPuzzleChar
                   , showPuzzle1
                   , showPuzzle
                   , printClipPuzzle
                   , showSolution
                   ) where

import           Config.Types  (Config (..))
import           Control.Monad (when)
import           Puzzle.Types
import           System.Hclip  (setClipboard)

showPuzzleChar :: Config -> PuzzleChar -> String
showPuzzleChar c (Exposed x)
  | equalCharSpan c = x : (' ' <$ drop 1 (hidden c))
  | otherwise = [x]
showPuzzleChar c (Censored _) = hidden c

showPuzzle1 :: Config -> Puzzle1 -> String
showPuzzle1 = concatMap . showPuzzleChar

showPuzzle :: Config -> Puzzle -> String
showPuzzle c p = unlines $ zipWith (listing c) [1..] (map (showPuzzle1 c) p)

showSolution :: Config -> Solution -> String
showSolution c s = unlines $ zipWith (listing c) [1..] s

-- | Print the puzzle to stdout
-- and copy the puzzle to user clipboard,
-- if confirmed in the user config.
printClipPuzzle :: Config -> Puzzle -> IO ()
printClipPuzzle c p = do when (clipboardPuzzle c) $ setClipboard str
                         putStrLn str
                           where str = showPuzzle c p
