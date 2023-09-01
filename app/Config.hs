{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Config ( userPrompt
              , dbPath
              , configPath
              , userConfig
              , Config(..)
              ) where

import           Control.Applicative       (asum)
import           Control.Monad             (guard)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Text                 (pack)
import           Dhall                     (FromDhall, Generic, Natural, auto,
                                            input)
import           Paths_chunizm             (getDataFileName)
import           System.Directory          (XdgDirectory (XdgConfig),
                                            doesFileExist, getHomeDirectory,
                                            getXdgDirectory)


import           System.Environment        (lookupEnv)
import           System.FilePath           ((<.>), (</>))

dbPath :: IO FilePath
dbPath = do c <- userConfig
            case db c of
              Just f  -> return f
              Nothing -> getResponse "Enter path to your db: "

userPrompt :: IO Prompt
userPrompt = prompt <$> userConfig

{- searching order:
- env var $CHUNIZM_CONFIG
- $XDG_CONFIG_HOME/chunizm/config.dhall (or ~/.config if env var not set)
- $HOME/.chunizm
-}
configPath :: IO (Maybe FilePath)
configPath = runMaybeT configPathT

configPathT :: MaybeT IO FilePath
configPathT = asum configPathsT

configPathsT :: [MaybeT IO FilePath]
configPathsT = [ MaybeT (lookupEnv envName)
               , do xdg <- lift $ getXdgDirectory XdgConfig ("chunizm/config" <.> configExtension)
                    exists <- lift $ doesFileExist xdg
                    guard exists
                    return xdg
               , (</> ".chunizm") <$> lift getHomeDirectory
               ]

userConfig :: IO Config
userConfig = do path <- configPath
                defaultPath <- getDataFileName "defaults.dhall"
                case path of
                  Nothing -> parseFile defaultPath
                  Just f  -> parseConfig (defaultPath ++ " // " ++ f)  -- DIRTY!


--- impl

type Prompt = String

data Config = Config { db                :: Maybe FilePath
                     , accentSensitive   :: Bool
                     , caseSensitive     :: Bool
                     , alphaNumOnly      :: Bool
                     , showSpace         :: Bool
                     , prompt            :: Prompt
                     , hidden            :: String
                     , defaultPuzzleSize :: Natural
                     , clipboardPuzzle   :: Bool
                     } deriving (Show, Generic, FromDhall, Eq)

parseConfig :: String -> IO Config
parseConfig = input auto . pack

parseFile :: FilePath -> IO Config
parseFile = parseConfig

getResponse :: String -> IO String
getResponse s = putStr s >> getLine

envName :: FilePath
envName = "CHUNIZM_CONFIG"

configExtension :: FilePath
configExtension = "dhall"
