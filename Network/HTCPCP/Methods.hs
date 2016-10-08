module Network.HTCPCP.Methods where

data PotRequestMethod = BREW
                      | GET
                      | PROPFIND
                      | WHEN
                      | Custom String
                      deriving (Eq)
instance Show PotRequestMethod where
    show x =
        case x of
            BREW       ->   "BREW" -- Post
            GET        ->   "GET"
            PROPFIND   ->   "PROPFIND"
            WHEN       ->   "WHEN"
