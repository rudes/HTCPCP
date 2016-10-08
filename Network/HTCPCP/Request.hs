module Network.HTCPCP.Request where

import Network.HTCPCP.Headers
import Network.HTCPCP.Methods
import Network.HTCPCP.URI

data PotRequest = PotRequest
    { prqURI :: PotURI
    , prqMethod :: PotRequestMethod
    , prqHeaders :: [PotHeader]
    , prqBody :: String } deriving (Eq)
instance Show PotRequest where
    show (PotRequest u m h b) =
        show m ++ " " ++ show u ++ " " ++ "\r\n"
        ++ concatMap show h ++ "\r\n" ++ b
