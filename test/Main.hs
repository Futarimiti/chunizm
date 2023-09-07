{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import           Canonical
import           System.Exit (die)
import           Test.HUnit
import Show (showExampleBoard)

-- | Tests are always failed on purpose
-- since failing a Test does not halt the program.
-- Read logs to see the test status.
main :: IO ()
main = do runTestTT normaliseThese
          runTestTT don'tNormaliseThese
          runTestTT matchThese
          -- roundBeforeAfter
          showExampleBoard
          die "TEST FINISHED"
