module Network.HTCPCP.PotSpec where

import Test.Hspec
import Network.HTCPCP.Pot

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "PotURI" $ it "builds a Coffee URI" $
        show PotURI { potUriScheme = "coffee", potUriHost = "host"
                    , potDesignator = 2, potAdditions = MILK WHOLEMILK
            } `shouldBe` ("coffee://host/pot-2#WHOLE-MILK" :: String)
    describe "PotRequest" $ it "builds a Coffee Request" $
        show PotRequest { prqURI = PotURI { potUriScheme = "coffee"
                                            , potUriHost = "host"
                                            , potDesignator = 2
                                            , potAdditions = MILK WHOLEMILK }
                        , prqMethod = GET
                        , prqHeaders = [PotHeader SAFE "yes"
                                        , PotHeader ACCEPT "WHOLE-MILK"]
                        , prqBody = "test" }
                        `shouldBe` ("GET coffee://host/pot-2#WHOLE-MILK \r\nSafe:yes;Accept-Additions:WHOLE-MILK;\r\ntest" :: String)
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
