{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Game.Chunizm.Core.Types where

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.RWS    (RWST)
import           Data.Map                   (Map)
import           Data.Set                   (Set)
import           Dhall                      (FromDhall, ToDhall)
import           GHC.Generics               (Generic)
import           Numeric.Natural            (Natural)
import           Text.Show.Functions        ()

-- | A REPL command.
type Command = Maybe String  -- | Args, unsplit since spacing may be significant.
            -> RWST Global () Round (ExceptT Error IO) Outcome

-- | A collection of commands.
type CommandSet = Map String  -- | name to invoke this command
                      Command

type Error = String

-- | Terrible name for global constants
-- Only its selector should be used in reader
data Global = Global { config :: Config
                     , info   :: Info
                     , errors :: Errors
                     , cmdset :: CommandSet
                     } deriving (Show)

-- | Error messages. To be read from an external resources.
data Errors = Errors { illegalArgsError   :: String -> String
                     , arityMismatchError :: Natural  -- | Expected
                                          -> Natural  -- | Actual
                                          -> String
                     , malformedCmdError :: String
                     } deriving (Show, Generic, FromDhall)

-- | To be read from an external resources.
data Info = Info { leaveMessage   :: String              -- | Displayed when the user exits REPL.
                 , welcomeMessage :: String              -- | Displayed when the user enters REPL.
                 , confirmation   :: String              -- | Displayed when the user needs to confirm a destructive decision.
                 , proceeding     :: String              -- | Asks if the user would like to proceed their decision.
                 , cancelConfirm  :: String              -- | Displayed when the user cancels a confirmation.
                 , emptyBoard     :: String              -- | Display this when trying to display an empty board.
                 , errorMessage   :: String              -- | Displayed when an unknown exception has occurred.
                 , version        :: [Natural]           -- | Current version represented in list of nats
                 , versionDisplay :: String -> String    -- | How to present the version information.
                 , dbDisplay      :: FilePath -> String  -- | How to present the path to data source.
                 } deriving (Show, Generic, FromDhall)

-- | Outcome of executing a REPL command.
data Outcome = Outcome { output    :: Maybe String  -- | Output message
                       , showBoard :: Bool          -- | Show board and related stuff (e.g. opened chars)?
                       , continue  :: Bool          -- | Continue REPL or end it?
                       } deriving (Show, Eq)

-- | User configuration schema.
data Config = Config { printComponents    :: [Component]  -- | To print which components and arranging them in which order
                     , clipComponents     :: [Component]  -- | Ditto, this time clipboard
                     , defaultBoardSize   :: Natural      -- | Default size of the puzzle board, used for quick generation
                     , dataSource         :: FilePath     -- | Path to db of songs.
                     , boardListing       :: String       -- | Decides how to format the listing of each puzzle on the board.
                                                          -- should include "%d" and "%s" format specifiers,
                                                          -- denoting the index and the puzzle.
                                                          -- example: @"%d. %s"@ gives @1. ****@
                     , hideSpace          :: Bool         -- | Decides whether spaces should be always exposed. Why not?
                     , caseSensitive      :: Bool         -- | Self-explanatory.
                     , accentSensitive    :: Bool         -- | Self-explanatory.
                     , confirmDestructive :: Bool         -- | Will need cofirmation on destructive moves.
                     , hiddenChar         :: String       -- | A *character* used in place of hidden characters.
                                                          -- Certain configuration languages do not support Char type
                                                          -- so String is used instead.
                                                          -- You should still use 1 character though, or the rest will be truncated.
                     , replPrompt         :: String       -- | Self-explanatory.
                     , suppressErrors     :: Bool         -- | Suppress errors messages? (highly unrecommended)
                     , betwComponents     :: String       -- | What to be intercalated between components when showing them?
                     , showVersionAtStart :: Bool         -- | Show version of Chunizm at the start of the game.
                     , showDBAtStart      :: Bool         -- | Show DB in use at the start of the game.
                     } deriving (Show, Eq, Generic, FromDhall, ToDhall)

-- | Used to specify relative positions of each component.
-- A component will show up / be clipped
-- iff 1) it is meant to be shown,
-- AND 2) its corresponding component is included
-- in @printComponents@/@clipComponents@.
data Component = OpenedChars
               | Board
               | Output
               -- | CurrentPlayer
               deriving (Show, Eq, Generic, FromDhall, ToDhall)

-- | Carrier for these components.
data Components = Components { openedChars :: Set Char
                             , mOutput     :: Maybe String
                             , theBoard    :: Board
                             } deriving (Show)

-- | Carries information for each round of the game.
data Round = Round { board  :: Board     -- | current board
                   -- , player :: Player    -- | current player (UNUSED)
                   , opened :: Set Char  -- | characters already opened until this round
                   } deriving (Show, Eq, Read)

-- | The board, containing numerous puzzles to be attempted.
type Board = [[GameChar]]

-- | Every puzzle consists of game chars.
-- A game char always represent a char,
-- and is either exposed or hidden (default).
data GameChar = Exposed Char
              | Hidden Char
              deriving (Show, Eq, Read, Ord)

-- | NOTE: currently unused.
-- A player, identified by a name.
newtype Player = Player { getName :: String }
  deriving (Show, Eq, Read, Generic, FromDhall, ToDhall)
