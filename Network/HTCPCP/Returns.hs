module Network.HTCPCP.Returns where

data PotErrorResponse = NA | TPOT
instance Show PotErrorResponse where
    show x =
        case x of
          NA -> "406 Not Acceptable"
          TPOT -> "418 I'm a Teapot"

