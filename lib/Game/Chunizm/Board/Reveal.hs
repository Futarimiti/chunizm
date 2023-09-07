{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Game.Chunizm.Board.Reveal where

import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Control.Monad.Trans.Reader     (ReaderT, asks)
import           Control.Monad.Trans.State      (StateT, gets, modify, put)
import           Data.Functor.Compose           (Compose (Compose, getCompose))
import           Data.Set                       (insert, member)
import           Game.Chunizm.Board.Reveal.Char (matchChar)
import           Game.Chunizm.Core.Types
import           Numeric.Natural                (Natural)
import           Safe                           (atMay)

reveal1 :: [GameChar] -> [GameChar]
reveal1 = map exposeGameChar

-- | Reveal the nth puzzle.
-- 1-based.
revealNth :: Monad m => Natural -> StateT Board m ()
revealNth (subtract 1 -> i) = modify $ updateAt i reveal1
  where -- | 0-based list update.
        updateAt :: Natural -> (a -> a) -> [a] -> [a]
        updateAt 0 f (x:xs) = f x:xs
        updateAt n f (x:xs)
          | n < 0 = x:xs
          | otherwise = x : updateAt (n - 1) f xs
        updateAt _ _ xs = xs

-- | Given a character @c@,
-- opens up all characters within a board that *matches* @c@,
-- and add @c@ to @opened@ if not already a member.
open :: (Monad f)
     => Char -> ReaderT Config (StateT Round f) ()
open c = do match <- matchChar
            let expose (Hidden ch) | match c ch = Exposed ch
                expose a                        = a
            o <- lift $ gets opened
            b <- lift $ gets board
            lift $ put Round { opened = insert c o
                             , board = if c `member` o
                                       then b
                                       else getCompose $ expose <$> Compose b
                             }

-- | Convert a board to list of strings, each corresponding to a solution.
getSolutions :: Board -> [String]
getSolutions = getCompose . fmap relieveGameChar . Compose

-- | 1-based.
getNthSolution :: Natural -> Board -> Maybe String
getNthSolution (subtract 1 . fromIntegral . toInteger -> n) b = getCompose $ fmap relieveGameChar $ Compose $ b `atMay` n

-- | Convert a board to list of strings, each corresponding to a puzzle.
getPuzzles :: Monad m => ReaderT Config m (Board -> [String])
getPuzzles = do display <- renderGameChar
                return (getCompose . fmap display . Compose)

exposeGameChar :: GameChar -> GameChar
exposeGameChar (Hidden ch) = Exposed ch
exposeGameChar x           = x

-- | Show exposed characters as-is while rendering hidden characters as @hiddenChar@.
renderGameChar :: Monad m => ReaderT Config m (GameChar -> Char)
renderGameChar = do hc <- asks hiddenChar
                    return $ \case Exposed ch -> ch
                                   Hidden _ -> head hc

-- | Take out a character from @GameChar@, regardless of being exposed or hidden.
-- Useful for getting the solution.
relieveGameChar :: GameChar -> Char
relieveGameChar (Exposed c) = c
relieveGameChar (Hidden c)  = c

