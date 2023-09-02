module Util where

import           Data.Char                  (isMark, toLower)
import           Data.List                  (genericTake)
import           Numeric.Natural            (Natural)
import           Rando                      (shuffle)
import           Unicode.Char.Normalization (DecomposeMode (..), decompose)

canonical :: String -> String
canonical = filter (not . isMark) . concatMap (decompose Canonical)

canon :: Char -> Char
canon = head . canonical . pure

(==?) :: String -> String -> Bool
(==?) x y = map toLower x == map toLower y

pick :: Natural -> [a] -> IO [a]
pick n xs = genericTake n <$> shuffle xs

-- | 0-based list update.
updateAt :: Natural -> (a -> a) -> [a] -> [a]
updateAt 0 f (x:xs) = f x:xs
updateAt n f (x:xs)
  | n < 0 = x:xs
  | otherwise = x : updateAt (n - 1) f xs
updateAt _ _ xs = xs

lookup' :: Eq a => a -> [([a], b)] -> Maybe b
lookup' _ [] = Nothing
lookup' key ((keys, v):xs)
  | key `elem` keys = Just v
  | otherwise = lookup' key xs

