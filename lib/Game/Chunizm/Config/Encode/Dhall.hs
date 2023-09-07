module Game.Chunizm.Config.Encode.Dhall where

import           Data.Void    (Void)
import           Dhall        (Encoder (embed), ToDhall, inject)
import           Dhall.Core   (Expr)
import           Dhall.Parser (Src)
import           Dhall.Pretty (prettyExpr)

encode :: ToDhall d => d -> Expr Src Void
encode = embed inject

encodeDhallText :: ToDhall d => d -> String
encodeDhallText = show . prettyExpr . encode
