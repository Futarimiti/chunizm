{-# OPTIONS_GHC -Wno-missing-fields #-}

module Canonical where

import           Control.Monad.Trans.Reader          (runReader)
import           Control.Monad.Trans.State
import qualified Data.Set                            as Set
import           Game.Chunizm.Board.Gen              (mkBoard)
import           Game.Chunizm.Board.Reveal           (open)
import           Game.Chunizm.Board.Reveal.Char      (canon, canonical,
                                                      matchChar)
import           Game.Chunizm.Board.Show             (showTheBoard)
import           Game.Chunizm.Config.Defaults.Values (defaults)
import           Game.Chunizm.Core.Types             hiding (showBoard)
import           Test.HUnit

-- Testing: canon, canonical

normaliseThese :: Test
normaliseThese = "Should normalise some accented characters" ~: TestList
    [ canonical "é" ~?= "e"
    , canonical "å" ~?= "a"
    , canonical "Á" ~?= "A"
    , canonical "ç" ~?= "c"
    , canonical "ö" ~?= "o"
    , canonical "ñ" ~?= "n"
    , canon 'é' ~?= "e"
    , canon 'å' ~?= "a"
    , canon 'Á' ~?= "A"
    , canon 'ç' ~?= "c"
    , canon 'ö' ~?= "o"
    , canon 'ñ' ~?= "n"
    ]

don'tNormaliseThese :: Test
don'tNormaliseThese = "Should not \"normalise\" certain distinct characters" ~: TestList
  [ canon 'ø' ~?= "ø" ]

-- Testing: matchChar

-- Example configs, tailored for testing @matchThese@.
-- Certain fields are delibrately left undefined;
-- they should not be used anyway.
sensitive :: Config
sensitive = Config { caseSensitive = True
                   , accentSensitive = True
                   }

insensitive :: Config
insensitive = Config { caseSensitive = False
                     , accentSensitive = False
                     }

onlyCase :: Config
onlyCase = insensitive { caseSensitive = True }

onlyAccent :: Config
onlyAccent = insensitive { accentSensitive = True }

matchUsing :: Config -> Char -> Char -> Bool
matchUsing = runReader matchChar

matchThese :: Test
matchThese = "Should match chars according to config" ~: TestList $
  -- exact match only
  [ matchUsing sensitive 'a' 'a' ~?= True
  , matchUsing sensitive 'a' 'A' ~?= False
  , matchUsing sensitive 'a' 'å' ~?= False
  ]
  <>
  -- case sensitive, accent insensitive
  [ matchUsing onlyCase 'a' 'a' ~?= True
  , matchUsing onlyCase 'a' 'A' ~?= False
  , matchUsing onlyCase 'a' 'å' ~?= True
  , matchUsing onlyCase 'a' 'Å' ~?= False
  ]
  <>
  -- accent sensitive, case insensitive
  [ matchUsing onlyAccent 'a' 'a' ~?= True
  , matchUsing onlyAccent 'a' 'A' ~?= True
  , matchUsing onlyAccent 'a' 'å' ~?= False
  , matchUsing onlyAccent 'A' 'Å' ~?= False
  ]
  <>
  -- everything should match
  [ matchUsing insensitive 'a' 'a' ~?= True
  , matchUsing insensitive 'a' 'A' ~?= True
  , matchUsing insensitive 'a' 'å' ~?= True
  , matchUsing insensitive 'A' 'Å' ~?= True
  ]

-- Testing: open

-- insensitiveRound :: Round
-- insensitiveRound = Round { board = mkBoard defaults ["Wah!", "MassacrE || 3rc4224M"]
--                          , opened = Set.empty
--                          }

-- openedRound :: Round
-- openedRound = execState (runReader (open 'w') defaults) insensitiveRound

-- roundBeforeAfter :: IO ()
-- roundBeforeAfter = do putStr "Before: "
--                       print (runReader showTheBoard defaults (board insensitiveRound))
--                       print (opened insensitiveRound)
--                       putStr "After: "
--                       print (runReader showTheBoard defaults (board openedRound))
--                       print (opened openedRound)
