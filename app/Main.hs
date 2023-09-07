module Main where

import           Game.Chunizm
import           Resources    (globals)

main :: IO ()
main = globals >>= chunizm
