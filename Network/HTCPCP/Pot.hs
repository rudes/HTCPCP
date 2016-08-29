module Network.HTCPCP.Pot where

data PotRequestMethod = BREW | GET | PROPFIND | WHEN | Custom String
data PotHeader = PotHeader PotHeaderName String
data PotErrorResponse = NA | TPOT
data PotSafeHeader = YES | NO | CONDITONAL Bool
data PotMilk = CREAM | HALFANDHALF | WHOLEMILK | PARTSKIM | SKIM | NONDAIRY
data PotSyrup = VANILLA | ALMOND | RASPBERRY | CHOCOLATE
data PotAlcohol = WHISKY | RUM | KAHLUA | AQUAVIT
data PotAddType = MILK PotMilk
                | SYRUP PotSyrup
                | ALCOHOL PotAlcohol
                | CustomAddition String
data PotHeaderName = SAFE | ACCEPT PotAddType
-- potUriScheme://potUriHost/potDesignator#potAdditions
-- coffee://host-address/pot-1#CREAM
data PotURI = PotURI
    { potUriScheme :: String
    , potUriHost :: String
    , potDesignator :: String
    , potAdditions :: PotAddType
    }
data PotRequest a =
    Request { prqURI :: PotURI
            , prqMethod :: PotRequestMethod
            , prqHeaders :: [PotHeader]
            , prqBody :: a }

instance Show PotURI where
    show (PotURI s h d a) = s ++ h ++ d ++ show a
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

instance Show PotHeaderName where
    show x =
        case x of
          SAFE -> "Safe"
          ACCEPT t -> show t

instance Show PotAddType where
    show x =
        case x of
          MILK m -> show m
          SYRUP s -> show s
          ALCOHOL a -> show a
          CustomAddition s -> s

instance Show PotMilk where
    show x =
        case x of
          CREAM -> "CREAM"
          HALFANDHALF -> "HALF-AND-HALF"
          WHOLEMILK -> "WHOLE-MILK"
          PARTSKIM -> "PART-SKIM"
          SKIM -> "SKIM"
          NONDAIRY -> "NON-DAIRY"

instance Show PotSyrup where
    show x =
        case x of
          VANILLA -> "VANILLA"
          ALMOND -> "ALMOND"
          RASPBERRY -> "RASPBERRY"
          CHOCOLATE -> "CHOCOLATE"

instance Show PotAlcohol where
    show x =
        case x of
          WHISKY -> "WHISKY"
          RUM -> "RUM"
          KAHLUA -> "KAHLUA"
          AQUAVIT -> "AQUAVIT"

