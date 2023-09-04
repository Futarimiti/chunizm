{-# LANGUAGE DerivingVia #-}

module Player.Types where

newtype Player = Player { name :: String }
  deriving (Show, Read, Eq) via String
