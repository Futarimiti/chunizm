{-# LANGUAGE ViewPatterns #-}

module Config ( configPath
              , userConfig
              , getUserDBPath
              , putPrompt
              ) where

import           Config.Types
import           Control.Applicative       (asum)
import           Control.Monad             (guard)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Text                 (pack)
import           Dhall                     (auto, input)
import           Paths_chunizm             (getDataFileName)
import           System.Directory          (XdgDirectory (XdgConfig),
                                            doesFileExist, getHomeDirectory,
                                            getXdgDirectory)
import           System.Environment        (lookupEnv)
import           System.FilePath           ((<.>), (</>))

putPrompt :: Config -> IO ()
putPrompt c = putStr (prompt c)

getUserDBPath :: Config -> IO FilePath
getUserDBPath (db -> Just f) = return f
getUserDBPath _              = getResponse "Enter path to your db: "

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
                maybe parseFile override path defaultPath


--- impl


-- | Dirty
override :: FilePath  -- user
         -> FilePath  -- default
         -> IO Config
override user def = parseConfig $ def ++ " // " ++ user

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
