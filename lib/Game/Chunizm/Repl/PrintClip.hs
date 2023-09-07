{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Chunizm.Repl.PrintClip where

import           Control.Monad.Trans.Reader (ReaderT, ask, asks, runReader,
                                             withReaderT)
import           Data.List                  (intercalate)
import           Data.Maybe                 (mapMaybe)
import           Game.Chunizm.Board.Show    (showTheBoard, showTheOpened)
import           Game.Chunizm.Core.Types
import           System.Hclip               (setClipboard)

printComps :: Monad m => ReaderT (Global, Outcome, Round) m (IO ())
printComps = do f <- doWithComps putStrLn
                comps <- asks (printComponents . config . _1)
                return (f comps)

clipComps :: Monad m => ReaderT (Global, Outcome, Round) m (IO ())
clipComps = do f <- doWithComps setClipboard
               comps <- asks (clipComponents . config . _1)
               return (f comps)

doWithComps :: (Monad m1, Monad m2) => (String -> m2 b) -> ReaderT (Global, Outcome, Round) m1 ([Component] -> m2 ())
doWithComps f = do render <- render1
                   cat <- withReaderT (config . _1) concatAll
                   return $ mapM_ f . cat . mapMaybe render

_1 :: (a, b, c) -> a
_1 (x, _, _) = x

-- | Concatenate all the components (now in string),
-- using @betwComponents@ to intercalate.
-- Returns @Nothing@ for @[]@.
concatAll :: Monad m => ReaderT Config m ([String] -> Maybe String)
concatAll = do betw <- asks betwComponents
               return $ \case [] -> Nothing
                              xs -> Just $ intercalate betw xs

-- | Given a component, extract the corresponding information
-- from @Outcome@ and @Round@, then show it
-- unless these is nothing to show about it,
-- or it has been set not to be shown somewhere.
render1 :: Monad m => ReaderT (Global, Outcome, Round) m (Component -> Maybe String)
render1 = do (g, Outcome{..}, Round{..}) <- ask
             return $ \case Board -> if showBoard then Just $ runReader showTheBoard g board else Nothing
                            OpenedChars -> if showBoard  -- why show opened chars when not the board
                                              then Just $ runReader showTheOpened (config g) opened else Nothing
                            Output -> output  -- put output whenever there is
