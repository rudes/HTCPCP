module Network.HTCPCP.PotSpec where

import Test.Hspec
import Network.HTCPCP.Pot

spec :: Spec
spec = do
    let defPotURI = PotURI {
        potUriScheme = "coffee"
        , potUriHost = "host"
        , potDesignator = 2
        , potAdditions = MILK WHOLEMILK }
    let defPotReq =  PotRequest {
        prqURI = defPotURI
        , prqMethod = GET
        , prqHeaders =
            [PotHeader SAFE "yes"
            , PotHeader ACCEPT "WHOLE-MILK"]
        , prqBody = "test" }
    describe "replacePotHeader" $
        it "replaces header in the headers" $
            replacePotHeader ACCEPT "SKIM" defPotReq
                `shouldBe` (PotRequest {
                prqURI = defPotURI
                , prqMethod = GET
                , prqHeaders =
                    [PotHeader ACCEPT "SKIM"
                    , PotHeader SAFE "yes"]
                , prqBody = "test" } :: PotRequest)
    describe "insPotHeaders" $
        it "appends headers to the head of the headers" $
            insPotHeaders [PotHeader SAFE "no"
                          , PotHeader ACCEPT "SKIM"] defPotReq
                `shouldBe` (PotRequest {
                prqURI = defPotURI
                , prqMethod = GET
                , prqHeaders =
                    [PotHeader SAFE "yes"
                    , PotHeader ACCEPT "WHOLE-MILK"
                    , PotHeader SAFE "no"
                    , PotHeader ACCEPT "SKIM"]
                , prqBody = "test" } :: PotRequest)
    describe "insPotHeader" $ it "appends header to the head of the headers" $
        insPotHeader SAFE "no" defPotReq
        `shouldBe` (PotRequest {
        prqURI = defPotURI
        , prqMethod = GET
        , prqHeaders =
            [PotHeader SAFE "no"
            , PotHeader SAFE "yes"
            , PotHeader ACCEPT "WHOLE-MILK"]
        , prqBody = "test" } :: PotRequest)
    describe "retPotHeaders" $ it "gets all Coffee Headers from x" $
        retPotHeaders defPotReq `shouldBe` ([PotHeader SAFE "yes"
          , PotHeader ACCEPT "WHOLE-MILK"] :: [PotHeader])
    describe "mkPotHeader" $ it "builds a Coffee Header" $
        show (mkPotHeader SAFE "yes")
        `shouldBe` ("Safe:yes;" :: String)
    describe "PotURI" $ it "builds a Coffee URI" $
        show defPotURI
        `shouldBe` ("coffee://host/pot-2#WHOLE-MILK" :: String)
    describe "PotRequest" $ it "builds a Coffee Request" $
        show defPotReq `shouldBe`
        ("GET coffee://host/pot-2#WHOLE-MILK \r\n\
            \Safe:yes;Accept-Additions:WHOLE-MILK;\r\ntest" :: String)
    describe "PotRequestMethod" $ it "test Requests Methods" $ do
        show BREW `shouldBe` ("BREW" :: String)
        show GET `shouldBe` ("GET" :: String)
        show PROPFIND `shouldBe` ("PROPFIND" :: String)
        show WHEN `shouldBe` ("WHEN" :: String)
    describe "PotAddType" $ it "test Addition Types" $ do
        show (MILK WHOLEMILK) `shouldBe` ("WHOLE-MILK" :: String)
        show (SYRUP VANILLA) `shouldBe` ("VANILLA" :: String)
        show (ALCOHOL WHISKY) `shouldBe` ("WHISKY" :: String)
        show (CustomAddition "SUGAR") `shouldBe` ("SUGAR" :: String)
    describe "PotMilk" $ it "test Milk Types" $ do
        show CREAM `shouldBe` ("CREAM" :: String)
        show HALFANDHALF `shouldBe` ("HALF-AND-HALF" :: String)
        show WHOLEMILK `shouldBe` ("WHOLE-MILK" :: String)
        show PARTSKIM `shouldBe` ("PART-SKIM" :: String)
        show SKIM `shouldBe` ("SKIM" :: String)
        show NONDAIRY `shouldBe` ("NON-DAIRY" :: String)
    describe "PotSyrup" $ it "test Syrup Types" $ do
        show VANILLA `shouldBe` ("VANILLA" :: String)
        show ALMOND `shouldBe` ("ALMOND" :: String)
        show RASPBERRY `shouldBe` ("RASPBERRY" :: String)
        show CHOCOLATE `shouldBe` ("CHOCOLATE" :: String)
    describe "PotAlcohol" $ it "test Alcohol Types" $ do
        show WHISKY `shouldBe` ("WHISKY" :: String)
        show RUM `shouldBe` ("RUM" :: String)
        show KAHLUA `shouldBe` ("KAHLUA" :: String)
        show AQUAVIT `shouldBe` ("AQUAVIT" :: String)
    describe "PotErrorResponse" $ it "builds a Coffee Error Response" $ do
        show NA `shouldBe` ("406 Not Acceptable" :: String)
        show TPOT `shouldBe` ("418 I'm a Teapot" :: String)
