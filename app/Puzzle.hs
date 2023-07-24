{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Puzzle ( Puzzle(..)
              , hiddenSymbol
              , showPuzzle
              , emptyPuzzle
              , attempt
              , reveal
              , mkPuzzle
              , printPuzzle
              , censor
              , labelledSolutions
              , solutions
              , selectFromDB
              ) where

import           Config          (Config (..), dbPath, userConfig)
import           Control.Monad   ((<=<))
import           Data.Char       (isLatin1, isSpace, toUpper)
import           Data.Function   (on)
import           Data.Functor    ((<&>))
import           Data.List       (genericTake)
import           Numeric.Natural (Natural)
import           Prelude         hiding (showChar)
import           Rando
import           Util            (canon, canonical, (<<$>>), (==?))

type Solution = String
type Clue = [Character]
newtype Puzzle = Puzzle { puzzle :: [(Clue, Solution)] } deriving (Show, Eq)

mkPuzzle :: [Solution] -> IO Puzzle
mkPuzzle sols = do clues <- mapM censor sols
                   return $ Puzzle $ zip clues sols

type Amount = Natural
selectFromDB :: Amount -> IO Puzzle
selectFromDB n = do onlyABC <- alphaNumOnly <$> userConfig
                    rawDB <- (dbPath >>= readFile) <&> lines
                    let db = if onlyABC then filter isABC rawDB else rawDB
                    ranDB <- shuffle db
                    mkPuzzle $ genericTake n ranDB

solutions :: Puzzle -> [Solution]
solutions = map snd . puzzle

labelledSolutions :: Puzzle -> String
labelledSolutions = unlines . enumerated . solutions

censor :: Solution -> IO Clue
censor sol = do censorSpace <- not . showSpace <$> userConfig
                if censorSpace then return . map (const Hidden) $ sol
                               else return . map (\c -> if isSpace c then Lit ' ' else Hidden) $ sol

showPuzzle :: Puzzle -> IO String
showPuzzle = fmap (unlines . enumerated) . mapM (showChars . fst) . puzzle

printPuzzle :: Puzzle -> IO ()
printPuzzle = putStrLn <=< showPuzzle

emptyPuzzle :: Puzzle
emptyPuzzle = Puzzle []

hiddenSymbol :: IO String
hiddenSymbol = userConfig <&> hidden

attempt :: String -> Puzzle -> IO (Maybe Puzzle)
attempt word (Puzzle p) = Puzzle <<$>> attempt' word p

attempt' :: String -> [([Character], Solution)] -> IO (Maybe [([Character], Solution)])
attempt' _ [] = return Nothing
attempt' word ((clue, sol):xs) = do match <- word `matches` sol
                                    if match then return $ Just $ (fromString sol, sol):xs
                                             else ((clue, sol):) <<$>> attempt' word xs

-- returns IO Bool since user settings may modify the result
matches :: String -> Solution -> IO Bool
matches word sol = do caseSen <- caseSensitive <$> userConfig
                      accentSen <- accentSensitive <$> userConfig
                      if caseSen && accentSen
                         then return $ word == sol
                         else if accentSen
                                 then return $ word ==? sol
                                 else if caseSen
                                         then return $ canonical word == canonical sol
                                         else return $ canonical word ==? canonical sol

reveal :: Char -> Puzzle -> IO Puzzle
reveal c (Puzzle{..}) = Puzzle <$> mapM (\(clue, sol) -> do resultChars <- revealClue c sol (=?) clue
                                                            return (resultChars, sol)) puzzle


revealClue :: Char -> Solution -> (Char -> Char -> IO Bool) -> [Character] -> IO [Character]
revealClue _ [] _ [] = return []
revealClue c (s:sol) cmp (Hidden:cs) = do matching <- cmp c s
                                          res <- revealClue c sol cmp cs
                                          if matching then return $ Lit s : res
                                                      else return $ Hidden : res
revealClue c (_:sol) cmp (lit:cs) = do res <- revealClue c sol cmp cs
                                       return $ lit : res
revealClue _ _ _ _ = fail "internal error: mismatched length of clue and solution"

(=?) :: Char -> Char -> IO Bool
x =? y = do caseSen <- caseSensitive <$> userConfig
            accentSen <- accentSensitive <$> userConfig
            let f = case (caseSen, accentSen) of
                      (True, True)   -> id
                      (True, False)  -> toUpper
                      (False, True)  -> canon
                      (False, False) -> toUpper . canon
            return $ ((==) `on` f) x y
--- impl

enumerated :: [String] -> [String]
enumerated = zipWith (++) labels
  where labels = (++ ". ") . show @Int <$> [1..]

data Character = Lit Char | Hidden
  deriving (Show, Eq)

fromString :: String -> [Character]
fromString = map Lit

showChar :: Character -> IO String
showChar (Lit c) = return [c]
showChar Hidden  = hiddenSymbol

showChars :: [Character] -> IO String
showChars = concatMapM showChar

concatMapM :: (Monad m, Traversable f) => (a -> m [b]) -> f a -> m [b]
concatMapM f xs = fmap concat (mapM f xs)

-- string is alphanumeric
-- not sure if covers everything
isABC :: String -> Bool
isABC = all isLatin1
