module Util (canonical, canon, (<<$>>), (==?)) where

import           Data.Char                  (isMark, toLower)
import           Unicode.Char.Normalization (DecomposeMode (..), decompose)

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

canonical :: String -> String
canonical = filter (not . isMark) . concatMap (decompose Canonical)

canon :: Char -> Char
canon = head . canonical . pure

(==?) :: String -> String -> Bool
(==?) x y = map toLower x == map toLower y
