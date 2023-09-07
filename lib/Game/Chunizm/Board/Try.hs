module Game.Chunizm.Board.Try where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader     (ReaderT (..))
import           Control.Monad.Trans.State      (StateT, get)
import           Data.Functor.Classes           (Eq1 (..))
import           Data.List                      (findIndex)
import           Game.Chunizm.Board.Reveal      (getSolutions, revealNth)
import           Game.Chunizm.Board.Reveal.Char (matchChar)
import           Game.Chunizm.Core.Types

-- | Looks into user configuration
-- for preferences on case & diacritics sensitive,
-- then decides whether two strings should form a match.
matchStr :: Monad m => ReaderT Config m (String -> String -> Bool)
matchStr = liftEq <$> matchChar

-- | Take an attempt.
-- If correct, reveal that entry on the board and return @True@.
-- Otherwise, return @False@, unchange the board.
attempt :: Monad m => String -> ReaderT Config (StateT Board m) Bool
attempt s = do match <- matchStr
               b <- lift get
               lift $ case findIndex (match s) (getSolutions b) of
                 Nothing -> return False
                 Just n  -> revealNth (fromIntegral n) >> return True

