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
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import           Data.Text                 (pack)
import           Dhall                     (FromDhall, auto, input)
import           GHC.Generics              (Generic)
import           Numeric.Natural           (Natural)
import           System.Directory          (XdgDirectory (XdgConfig),
                                            doesFileExist, getHomeDirectory,
                                            getXdgDirectory)
import           System.Environment        (lookupEnv)
import           System.FilePath           ((<.>), (</>))

dbPath :: IO FilePath
dbPath = db <$> userConfig

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
configPathsT = [ MaybeT . lookupEnv $ envName
               , do xdg <- lift . getXdgDirectory XdgConfig $ "chunizm/config" <.> configExtension
                    exists <- lift $ doesFileExist xdg
                    guard exists
                    return xdg
               , (</> ".chunizm") <$> lift getHomeDirectory
               ]

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

envName :: FilePath
envName = "CHUNIZM_CONFIG"

configExtension :: FilePath
configExtension = "dhall"
