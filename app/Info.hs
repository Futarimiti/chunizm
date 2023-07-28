{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Info (info, Info(..), putInfoLn) where

import           Config          (configPath, dbPath)
import           Data.Version    (makeVersion, showVersion)
import           Dhall           (FromDhall, Generic, auto, input)
import           Numeric.Natural (Natural)
import           Text.Printf     (printf)

data Info = Info { version    :: [Natural]
                 , desc       :: String
                 , configInfo :: String
                 , dbInfo     :: String
                 , endMsg     :: String
                 } deriving (Show, Generic, FromDhall)

info :: IO Info
info = input auto "./resources/info.dhall"

versionStr :: IO String
versionStr = showVersion . makeVersion . map fromIntegral . version <$> info

putInfoLn :: IO ()
putInfoLn = do putDescLn
               putConfigInfoLn
               putUsingDBLn

putUsingDBLn :: IO ()
putUsingDBLn = do dbInfoRaw <- dbInfo <$> info
                  db <- dbPath
                  putStrLn $ printf dbInfoRaw db

putConfigInfoLn :: IO ()
putConfigInfoLn = do confInfoRaw <- configInfo <$> info
                     res <- maybe "" (printf confInfoRaw) <$> configPath
                     putStrLn res

putDescLn :: IO ()
putDescLn = do descRaw <- desc <$> info
               ver <- versionStr
               putStrLn $ printf descRaw ver

