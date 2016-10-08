module Network.HTCPCP.Headers where

-------------------------------------------
-- From rfc2324:
--
-- Safe  = "Safe" ":" safe-nature
-- safe-nature = "yes" | "no" | conditionally-safe
-- conditionally-safe = "if-" safe-condition
-- safe-condition = "user-awake" | token
--
--
-- Accept-Additions = "Accept-Additons" ":" [addition-type]
-- addition-type = milk-type | syrup-type | sweetener-type 
--              | alcohol-type | "*"
--              (semi-colon seperated list)
-- milk-type = "Cream" | "Half-and-half" | "Whole-milk"
--              | "Part-skim" | "Skim" | "Non-Dairy"
-- syrup-type = "Vanilla" | "Almond" | "Raspberry" | "Chocolate"
-- sweetener-type = "Sugar" | "Honey"
-- alcohol-type = "Whisky" | "Rum" | "Kahlua" | "Aquavit"
--
-- Content-Type = "Content-Type" ":" "message/coffeepot"
--                  (Post and Brew require this header)
-------------------------------------------

import Network.HTCPCP.Methods

data PotURI = PotURI
    { potUriScheme :: String
    , potUriHost :: String
    , potDesignator :: Int
      , potAdditions :: PotAddType } deriving (Eq)
instance Show PotURI where
    show (PotURI s h d a) = s ++ "://" ++ h ++ "/pot-"
                            ++ show d ++ "#" ++ show a


data PotRequest = PotRequest
    { prqURI :: PotURI
    , prqMethod :: PotRequestMethod
    , prqHeaders :: [PotHeader]
    , prqBody :: String } deriving (Eq)
instance Show PotRequest where
    show (PotRequest u m h b) =
        show m ++ " " ++ show u ++ " " ++ "\r\n"
        ++ concatMap show h ++ "\r\n" ++ b
data PotAddType = MILK PotMilk
                | SYRUP PotSyrup
                | ALCOHOL PotAlcohol
                | CustomAddition String
                deriving (Eq)

class HasPotHeaders x where
    getPotHeaders :: x -> [PotHeader]
    setPotHeaders :: x -> [PotHeader] -> x
data PotHeader = PotHeader PotHeaderName String deriving (Eq)
instance Show PotHeader where
    show (PotHeader a x)  = show a ++ ":" ++ x ++ ";"
instance HasPotHeaders PotRequest where
    getPotHeaders = prqHeaders
    setPotHeaders prq phdrs = prq { prqHeaders=phdrs }
data PotHeaderName = SAFE | ACCEPT deriving (Eq)
instance Show PotHeaderName where
    show x =
        case x of
          SAFE -> "Safe"
          ACCEPT -> "Accept-Additions"

type PotHeaderSetter a = PotHeaderName -> String -> a -> a
mkPotHeader :: PotHeaderName -> String -> PotHeader
mkPotHeader = PotHeader

retPotHeaders :: HasPotHeaders a => a -> [PotHeader]
retPotHeaders = getPotHeaders

insPotHeader :: HasPotHeaders a => PotHeaderSetter a
insPotHeader name val x = setPotHeaders x newHeader
    where newHeader = PotHeader name val : getPotHeaders x

insPotHeaders :: HasPotHeaders a => [PotHeader] -> a -> a
insPotHeaders hdrs x = setPotHeaders x (getPotHeaders x ++ hdrs)

replacePotHeader :: HasPotHeaders a => PotHeaderSetter a
replacePotHeader name val h = setPotHeaders h newHead
    where newHead = PotHeader name val : [ x | x@(PotHeader n _) <-
              getPotHeaders h, name /= n]

instance Show PotAddType where
    show x =
        case x of
          MILK m -> show m
          SYRUP s -> show s
          ALCOHOL a -> show a
          CustomAddition s -> s

data PotMilk = CREAM
      | HALFANDHALF
      | WHOLEMILK
      | PARTSKIM
      | SKIM
      | NONDAIRY
      deriving (Eq)
instance Show PotMilk where
    show x =
        case x of
          CREAM -> "CREAM"
          HALFANDHALF -> "HALF-AND-HALF"
          WHOLEMILK -> "WHOLE-MILK"
          PARTSKIM -> "PART-SKIM"
          SKIM -> "SKIM"
          NONDAIRY -> "NON-DAIRY"

data PotSyrup = VANILLA | ALMOND | RASPBERRY | CHOCOLATE deriving (Eq)
instance Show PotSyrup where
    show x =
        case x of
          VANILLA -> "VANILLA"
          ALMOND -> "ALMOND"
          RASPBERRY -> "RASPBERRY"
          CHOCOLATE -> "CHOCOLATE"

data PotAlcohol = WHISKY | RUM | KAHLUA | AQUAVIT deriving (Eq)
instance Show PotAlcohol where
    show x =
        case x of
          WHISKY -> "WHISKY"
          RUM -> "RUM"
          KAHLUA -> "KAHLUA"
          AQUAVIT -> "AQUAVIT"

