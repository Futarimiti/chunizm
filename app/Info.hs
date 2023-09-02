{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Info (info, Info(..), putInfoLn) where

import           Config          (configPath, getUserDBPath)
import           Config.Types    (Config)
import           Data.Text       (pack)
import           Data.Version    (makeVersion, showVersion)
import           Dhall           (FromDhall, Generic, auto, input)
import           Numeric.Natural (Natural)
import           Paths_chunizm   (getDataFileName)
import           Text.Printf     (printf)

data Info = Info { version    :: [Natural]
                 , desc       :: String
                 , configInfo :: String
                 , dbInfo     :: String
                 , endMsg     :: String
                 } deriving (Show, Generic, FromDhall)

info :: IO Info
info = getDataFileName "info.dhall" >>= input auto . pack

versionStr :: IO String
versionStr = showVersion . makeVersion . map fromIntegral . version <$> info

putInfoLn :: Config -> IO ()
putInfoLn c = do putDescLn
                 putConfigInfoLn
                 putUsingDBLn c

putUsingDBLn :: Config -> IO ()
putUsingDBLn c = do dbInfoRaw <- dbInfo <$> info
                    db <- getUserDBPath c
                    putStrLn $ printf dbInfoRaw db

putConfigInfoLn :: IO ()
putConfigInfoLn = do confInfoRaw <- configInfo <$> info
                     res <- maybe "" (printf confInfoRaw) <$> configPath
                     putStrLn res

putDescLn :: IO ()
putDescLn = do descRaw <- desc <$> info
               ver <- versionStr
               putStrLn $ printf descRaw ver

