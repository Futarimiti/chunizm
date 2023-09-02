module Error where

import           Data.List   (intercalate)
import           Text.Printf (PrintfArg, printf)

type ErrMsg = String
type Arg = String
type Cmd = String

arityMismatch :: (Integral n, PrintfArg n) => n -> n -> ErrMsg
arityMismatch = printf "*** arity mismatch: expected %d, received %d"

aritiesMismatch :: (Integral n, PrintfArg n, Show n) => [n] -> n -> ErrMsg
aritiesMismatch ar = printf "*** arity mismatch: expected %s, received %d" (intercalate " or " (map show ar))

cmdNotFound :: Cmd -> ErrMsg
cmdNotFound = printf "*** command not found: %s"

illegalArg :: Arg -> ErrMsg
illegalArg = printf "*** illegal argument: %s"

canOnlyUncover1Letter :: ErrMsg
canOnlyUncover1Letter = "*** only one letter may be uncovered each time"
