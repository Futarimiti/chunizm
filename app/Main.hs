module Main (main) where

import Repl (repl)
import Info (putInfoLn)

main :: IO ()
main = do putInfoLn
          repl

