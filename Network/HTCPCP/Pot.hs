module Network.HTCPCP.Pot where

import Network.URI

data PotRequestMethod = BREW | GET | PROPFIND | WHEN
data PotHeader = PotHeader PotHeaderName String
data PotErrorResponse = NA | TPOT
data PotHeaderName = SAFE | ACCEPT
data PotRequest a =
    Request { prqURI :: URI
            , prqMethod :: PotRequestMethod
            , prqHeaders :: [PotHeader]
            , prqBody :: a }

instance Show PotRequestMethod where
    show x =
        case x of
            BREW       ->   "BREW" -- Post
            GET        ->   "GET"
            PROPFIND   ->   "PROPFIND"
            WHEN       ->   "WHEN"

instance Show PotErrorResponse where
    show x =
        case x of
          NA -> "406 Not Acceptable"
          TPOT -> "418 I'm a Teapot"
