module Network.HTCPCP.Base where

import Network.HTCPCP.Headers

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

instance HasPotHeaders PotRequest where
    getPotHeaders = prqHeaders
    setPotHeaders prq phdrs = prq { prqHeaders=phdrs }

data PotRequest = PotRequest
    { prqURI :: PotURI
    , prqMethod :: PotRequestMethod
    , prqHeaders :: [PotHeader]
    , prqBody :: String } deriving (Eq)

instance Show PotRequest where
    show (PotRequest u m h b) =
        show m ++ " " ++ show u ++ " " ++ "\r\n"
        ++ concatMap show h ++ "\r\n" ++ b

data PotURI = PotURI
    { potUriScheme :: String
    , potUriHost :: String
    , potDesignator :: Int
      , potAdditions :: PotAddType } deriving (Eq)
instance Show PotURI where
    show (PotURI s h d a) = s ++ "://" ++ h ++ "/pot-"
                            ++ show d ++ "#" ++ show a
