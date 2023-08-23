{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

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

import           Config                    (Config (..), dbPath, userConfig)
import           Control.Applicative       (Alternative (empty))
import           Control.Monad             (liftM2, (>=>))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Bool                 (bool)
import           Data.Char                 (isLatin1, isSpace, toUpper)
import           Data.Function             (on)
import           Data.Functor              ((<&>))
import           Data.Functor.Compose      (Compose (..))
import           Data.List                 (genericTake)
import           Numeric.Natural           (Natural)
import           Prelude                   hiding (showChar)
import           Rando                     (shuffle)
import           System.Hclip              (setClipboard)
import           Util                      (canon, canonical)

mkPuzzle :: [Solution] -> IO Puzzle
mkPuzzle sols = Puzzle . (`zip` sols) <$> mapM censor sols

type Amount = Natural
selectFromDB :: Amount -> IO Puzzle
selectFromDB n = userConfig >>= (\onlyABC -> dbPath >>= readFile >>= ((shuffle . bool id (filter isABC) onlyABC) >=> (mkPuzzle . genericTake n)) . lines) . alphaNumOnly

solutions :: Puzzle -> [Solution]
solutions = map snd . puzzle

labelledSolutions :: Puzzle -> String
labelledSolutions = unlines . enumerated . solutions

censor :: Solution -> IO Clue
censor = (userConfig >>=) . (. showSpace) . liftM2 bool (return . map (const Hidden)) (return . map (bool Hidden (Lit ' ') . isSpace))

showPuzzle :: Puzzle -> IO String
showPuzzle = fmap (unlines . enumerated) . mapM (showChars . fst) . puzzle

printPuzzle :: Puzzle -> IO ()
printPuzzle = (userConfig >>=) . (. clipboardPuzzle) . liftM2 bool (showPuzzle >=> putStrLn)
                                                                   (showPuzzle >=> liftM2 (>>) putStrLn setClipboard)

emptyPuzzle :: Puzzle
emptyPuzzle = Puzzle []

hiddenSymbol :: IO String
hiddenSymbol = userConfig <&> hidden

attempt :: [String] -> Puzzle -> IO (Maybe Puzzle)
attempt = (runMaybeT .) . attemptT

attemptT :: [String] -> Puzzle -> MaybeT IO Puzzle
attemptT _ (Puzzle [])               = empty
attemptT w (Puzzle ((clue, sol):xs)) = do matched <- lift $ w `matches` sol
                                          if matched then return . Puzzle $ (strToChars sol, sol) : xs
                                                    else do Puzzle res <- attemptT w (Puzzle xs)
                                                            return . Puzzle $ (clue, sol) : res

-- returns IO Bool since user settings may modify the result
matches :: [String] -> Solution -> IO Bool
matches w sol = do caseSen <- caseSensitive <$> userConfig
                   accentSen <- accentSensitive <$> userConfig
                   let f = case (caseSen, accentSen) of
                             (True, True) -> (==)
                             (True, False) -> (==) `on` fmap toUpper . Compose
                             (False, True) -> (==) `on` fmap canonical
                             (False, False) -> (==) `on` fmap toUpper . Compose . fmap canonical
                   return $ f w (words sol)

reveal :: Char -> Puzzle -> IO Puzzle
reveal c = fmap Puzzle . mapM (\(clue, sol) -> revealClue c sol (=?) clue <&> (, sol)) . puzzle

revealClue :: Char -> Solution -> (Char -> Char -> IO Bool) -> [Character] -> IO [Character]
revealClue _ [] _ [] = return []
revealClue c (s:sol) cmp (Hidden:cs) = cmp c s >>= (<$> revealClue c sol cmp cs) . bool (Hidden :) (Lit s :)
revealClue c (_:sol) cmp (lit:cs) = revealClue c sol cmp cs <&> (lit :)
revealClue _ _ _ _ = fail "internal error: mismatched length of clue and solution"

(=?) :: Char -> Char -> IO Bool
x =? y = do caseSen <- caseSensitive <$> userConfig
            accentSen <- accentSensitive <$> userConfig
            return $ ((==) `on` case (caseSen, accentSen) of
                                  (True, True)   -> id
                                  (True, False)  -> toUpper
                                  (False, True)  -> canon
                                  (False, False) -> toUpper . canon) x y
--- impl

type Solution = String
type Clue = [Character]
newtype Puzzle = Puzzle { puzzle :: [(Clue, Solution)] } deriving (Show, Eq)

enumerated :: [String] -> [String]
enumerated = zipWith (++) labels
  where labels = [show x ++ ". " | x :: Int <- [1..]]

data Character = Lit Char | Hidden
  deriving (Show, Eq)

strToChars :: String -> [Character]
strToChars = map Lit

showChar :: Character -> IO String
showChar (Lit c) = return [c]
showChar Hidden  = hiddenSymbol

showChars :: [Character] -> IO String
showChars = concatMapM showChar

concatMapM :: (Monad m, Traversable f) => (a -> m [b]) -> f a -> m [b]
concatMapM = (fmap concat .) . mapM

-- string is alphanumeric
-- not sure if covers everything
isABC :: String -> Bool
isABC = all isLatin1
