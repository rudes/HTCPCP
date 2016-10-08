module Network.HTCPCP.URI where

import Network.HTCPCP.Headers

data PotURI = PotURI
    { potUriScheme :: String
    , potUriHost :: String
    , potDesignator :: Int
      , potAdditions :: PotAddType } deriving (Eq)
instance Show PotURI where
    show (PotURI s h d a) = s ++ "://" ++ h ++ "/pot-"
                            ++ show d ++ "#" ++ show a

