module Network.HTCPCP.Pot where

data PotRequest = PotRequest
    { prqURI :: PotURI
    , prqMethod :: PotRequestMethod
    , prqHeaders :: [PotHeader]
    , prqBody :: String }
instance Show PotRequest where
    show (PotRequest u m h b) =
        show m ++ " " ++ show u ++ " " ++ "\r\n"
        ++ concatMap show h ++ "\r\n" ++ b


data PotRequestMethod = BREW | GET | PROPFIND | WHEN | Custom String
instance Show PotRequestMethod where
    show x =
        case x of
            BREW       ->   "BREW" -- Post
            GET        ->   "GET"
            PROPFIND   ->   "PROPFIND"
            WHEN       ->   "WHEN"

data PotURI = PotURI
    { potUriScheme :: String
    , potUriHost :: String
    , potDesignator :: Int
    , potAdditions :: PotAddType }
instance Show PotURI where
    show (PotURI s h d a) = s ++ "://" ++ h ++ "/pot-"
                            ++ show d ++ "#" ++ show a

data PotAddType = MILK PotMilk
                | SYRUP PotSyrup
                | ALCOHOL PotAlcohol
                | CustomAddition String

instance Show PotAddType where
    show x =
        case x of
          MILK m -> show m
          SYRUP s -> show s
          ALCOHOL a -> show a
          CustomAddition s -> s

data PotMilk = CREAM | HALFANDHALF | WHOLEMILK | PARTSKIM | SKIM | NONDAIRY
instance Show PotMilk where
    show x =
        case x of
          CREAM -> "CREAM"
          HALFANDHALF -> "HALF-AND-HALF"
          WHOLEMILK -> "WHOLE-MILK"
          PARTSKIM -> "PART-SKIM"
          SKIM -> "SKIM"
          NONDAIRY -> "NON-DAIRY"

data PotSyrup = VANILLA | ALMOND | RASPBERRY | CHOCOLATE
instance Show PotSyrup where
    show x =
        case x of
          VANILLA -> "VANILLA"
          ALMOND -> "ALMOND"
          RASPBERRY -> "RASPBERRY"
          CHOCOLATE -> "CHOCOLATE"

data PotAlcohol = WHISKY | RUM | KAHLUA | AQUAVIT
instance Show PotAlcohol where
    show x =
        case x of
          WHISKY -> "WHISKY"
          RUM -> "RUM"
          KAHLUA -> "KAHLUA"
          AQUAVIT -> "AQUAVIT"

data PotHeader = PotHeader PotHeaderName String
instance Show PotHeader where
    show (PotHeader a x)  = show a ++ ":" ++ x ++ ";"
data PotHeaderName = SAFE | ACCEPT
instance Show PotHeaderName where
    show x =
        case x of
          SAFE -> "Safe"
          ACCEPT -> "Accept-Additions"

data PotErrorResponse = NA | TPOT
instance Show PotErrorResponse where
    show x =
        case x of
          NA -> "406 Not Acceptable"
          TPOT -> "418 I'm a Teapot"

