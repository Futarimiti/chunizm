{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Config.Types where

import           Dhall           (FromDhall)
import           GHC.Generics    (Generic)
import           Numeric.Natural (Natural)

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
                     , equalCharSpan     :: Bool
                     , listing           :: Natural -> String -> String
                     } deriving (Generic, FromDhall)

