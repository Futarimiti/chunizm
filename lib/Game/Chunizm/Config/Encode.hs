module Game.Chunizm.Config.Encode where

import           Dhall                            (ToDhall)
import           Game.Chunizm.Config.Encode.Dhall (encodeDhallText)

encodeText :: ToDhall d => d -> String
encodeText = encodeDhallText
