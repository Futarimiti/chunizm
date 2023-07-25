module Info (endMsg, putInfoLn) where

import           Config       (configPath, dbPath)
import           Data.Functor ((<&>))
import qualified Data.Version as V
import           Text.Printf  (printf)

version :: IO Version
version = return $ mkVersion [1, 0]

versionStr :: IO String
versionStr = showVersion <$> version

desc :: IO String
desc = printf "Chunizm, version %s" <$> versionStr

configInfo :: IO String
configInfo = configPath <&> maybe "" (printf "Loaded configuration file from %s")

endMsg :: IO String
endMsg = return "Leaving Chunizm."

putInfoLn :: IO ()
putInfoLn = do putDescLn
               putConfigInfoLn
               putUsingDBLn

putUsingDBLn :: IO ()
putUsingDBLn = dbPath >>= putStrLn . printf "Using DB at %s"

putConfigInfoLn :: IO ()
putConfigInfoLn = configInfo >>= putStrLn

putDescLn :: IO ()
putDescLn = desc >>= putStrLn

--- impl

type Version = V.Version

mkVersion :: [Int] -> V.Version
mkVersion = V.makeVersion

showVersion :: V.Version -> String
showVersion = V.showVersion
