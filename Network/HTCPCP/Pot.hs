module Network.HTCPCP.Pot where

import Network.URI

data PotRequestMethod = BREW | GET | PROPFIND | WHEN
data PotHeader = PotHeader PotHeaderName String
data PotHeaderName =
    SAFE
data PotRequest a =
    Request { prqURI :: URI
            , prqMethod :: PotRequestMethod
            , prqHeaders :: [PotHeader]
            , prqBody :: a }

instance Show PotRequestMethod where
    show x =
        case x of
            BREW       ->   "BREW"
            GET        ->   "GET"
            PROPFIND   ->   "PROPFIND"
            WHEN       ->   "WHEN"
