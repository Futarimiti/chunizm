{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Config ( userPrompt
              , dbPath
              , configPath
              , userConfig
              , Config(..)
              ) where

import           Control.Applicative (asum)
import           Control.Monad       (filterM)
import           Data.Text           (pack)
import           Dhall               (FromDhall, auto, input)
import           GHC.Generics        (Generic)
import           Numeric.Natural     (Natural)
import           System.Directory    (doesFileExist)
import           System.Environment  (lookupEnv)
import           System.FilePath     ((<.>), (</>))
import           Util                ((<<$>>))

dbPath :: IO FilePath
dbPath = db <$> userConfig

userPrompt :: IO Prompt
userPrompt = prompt <$> userConfig

{- searching order:
- env var $CHUNIZM_CONFIG
- if $XDG set: $XDG/chunizm/config.dhall
  else $HOME/.config/chunizm/config.dhall
- $HOME/.chunizm
-}
configPath :: IO (Maybe FilePath)
configPath = configPaths >>= fmap asum . filterM (maybe (return False) doesFileExist)

configPaths :: IO [Maybe FilePath]
configPaths = sequence [ lookupEnv "CHUNIZM_CONFIG"
                       , (</> "chunizm/config" <.> ex) <<$>> xdg
                       , (</> ".config/chunizm/config" <.> ex) <<$>> home
                       , (</> ".chunizm") <<$>> home
                       ] where home = lookupEnv "HOME"
                               xdg = lookupEnv "XDG_CONFIG_HOME"
                               ex = configExtension

configExtension :: String
configExtension = "dhall"

userConfig :: IO Config
userConfig = configPath >>= maybe defaultConfig parseConfig

defaultConfig :: IO Config
defaultConfig = do db' <- getResponse "Input db: "
                   return $ Config { db = db'
                                   , accentSensitive = False
                                   , caseSensitive = False
                                   , alphaNumOnly = True
                                   , showSpace = True
                                   , prompt = ">> "
                                   , hidden = "*"
                                   , defaultPuzzleSize = 10
                                   , clipboardPuzzle = False
                                   }

--- impl

type Prompt = String

data Config = Config { db                :: String
                     , accentSensitive   :: Bool
                     , caseSensitive     :: Bool
                     , alphaNumOnly      :: Bool
                     , showSpace         :: Bool
                     , prompt            :: Prompt
                     , hidden            :: String
                     , defaultPuzzleSize :: Natural
                     , clipboardPuzzle   :: Bool
                     } deriving (Show, Generic, FromDhall, Eq)

parseConfig :: FilePath -> IO Config
parseConfig = input auto . pack

getResponse :: String -> IO String
getResponse s = putStr s >> getLine
