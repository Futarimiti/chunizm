{-# OPTIONS_GHC -Werror=missing-fields #-}

module Resources (globals) where

import           Control.Monad.Trans.Class             (MonadTrans (lift))
import           Control.Monad.Trans.Maybe             (MaybeT (..))
import           Data.Functor                          ((<&>))
import           Data.Map                              (fromList)
import           Dhall                                 (auto, inputFile)
import           Game.Chunizm.Config.Encode            (encodeText)
import           Game.Chunizm.Config.Load              (loadConfig)
import           Game.Chunizm.Config.Override          (override)
import           Game.Chunizm.Core.Types
import           Game.Chunizm.Repl.Commands.Collection
import           Paths_chunizm                         (getDataFileName)
import           System.Directory                      (XdgDirectory (..),
                                                        getHomeDirectory,
                                                        getXdgDirectory)
import           System.FilePath                       ((</>))

-- | Prepare at the start of game.
globals :: IO Global
globals = do i <- information
             c <- finalConfig
             e <- errs
             return Global {cmdset=commandSet, info=i, errors=e, config=c}

getUserConfig :: MaybeT IO FilePath
getUserConfig = do paths <- lift userConfigPaths
                   loadConfig paths

finalConfig :: IO Config
finalConfig = do u <- runMaybeT getUserConfig
                 maybe (return defaults) (`override` encodeText defaults) u


userConfigPaths :: IO [FilePath]
userConfigPaths = sequence [ getXdgDirectory XdgConfig "chunizm/config.dhall"
                           , getHomeDirectory <&> (</> ".chunizm")
                           ]

errs :: IO Errors
errs = getDataFileName "errors.dhall" >>= inputFile auto

information :: IO Info
information = getDataFileName "info.dhall" >>= inputFile auto

commandSet :: CommandSet
commandSet = fromList [ ("open", uncover)
                      , ("try", try)
                      , ("new", new)
                      , ("show", display)
                      , ("sol", sol)
                      , ("solutions", sol)
                      , ("restart", restart)
                      , ("exit", exit)
                      ] where sol = const $ display (Just "solutions")

defaults :: Config
defaults = Config { printComponents = [ Output
                                      , OpenedChars
                                      , Board
                                      ]
                  , clipComponents = [ OpenedChars
                                     , Board
                                     ]
                  , defaultBoardSize = 10
                  , dataSource = "You must specify a data source for me to generate a board!"
                  , boardListing = "%d. %s"
                  , hideSpace = True
                  , caseSensitive = False
                  , accentSensitive = False
                  , confirmDestructive = True
                  , hiddenChar = "*"
                  , replPrompt = ">> "
                  , suppressErrors = False
                  , betwComponents = "\n\n"
                  }
