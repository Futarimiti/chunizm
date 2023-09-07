module Game.Chunizm.Board.Reveal.Char
  ( canonical
  , canon
  , matchChar
  ) where

import           Control.Monad              (unless)
import           Control.Monad.Trans.Reader (ReaderT, asks)
import           Control.Monad.Trans.Writer (Writer, execWriter, tell)
import           Data.Char                  (isMark, toUpper)
import           Data.Function              (on)
import           Data.Monoid                (Endo (..))
import           Game.Chunizm.Core.Types
import           Unicode.Char.Normalization (DecomposeMode (..), decompose)

-- | @concatMap@ @canon@ over a string.
canonical :: String -> String
canonical = concatMap canon

-- | Attempt to canonicalise a diacritical character
-- by "decomposing" and then removing its accents.
-- The result is a string, which _should_ consist of
-- only 1 character but I'm not 100% sure.
-- *NOTE* : some characters like @'ø'@ and @'o'@, albeit look alike,
-- are distinct from each other and are not supposed to be canonicalised,
-- hence cannot be handled by simple normalisations.
canon :: Char -> String
canon = filter (not . isMark) . decompose Canonical

-- | Looks into user configuration
-- for preferences on case & diacritics sensitive,
-- then decides whether two chars should form a match.
matchChar :: Monad m => ReaderT Config m (Char -> Char -> Bool)
matchChar = do cs <- asks caseSensitive
               as <- asks accentSensitive
               let f = appEndo . execWriter $ matchWith cs as
               return ((==) `on` f . pure)

-- | Matching two strings (which should really be just 1 character)
-- given sensitivity options.
matchWith :: Bool  -- case sensitive?
          -> Bool  -- diacritic sensitive?
          -> Writer (Endo String) ()
matchWith c d = do unless c $ tell (Endo (map toUpper))
                   unless d $ tell (Endo canonical)
