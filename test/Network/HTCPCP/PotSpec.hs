module Network.HTCPCP.PotSpec where

import Test.Hspec
import Network.HTCPCP.Pot

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "PotURI" $
        it "builds a Coffee URI" $
            show PotURI { potUriScheme = "coffee", potUriHost = "host"
                        , potDesignator = 2, potAdditions = MILK WHOLEMILK
                } `shouldBe` ("coffee://host/pot-2#WHOLE-MILK" :: String)
    describe "PotRequest" $
        it "builds a Coffee Request" $
            show PotRequest { prqURI = PotURI { potUriScheme = "coffee"
                                              , potUriHost = "host"
                                              , potDesignator = 2
                                              , potAdditions = MILK WHOLEMILK }
                            , prqMethod = GET
                            , prqHeaders = [PotHeader SAFE "yes"
                                           , PotHeader ACCEPT "WHOLE-MILK"]
                            , prqBody = "test" }
                            `shouldBe` ("GET coffee://host/pot-2#WHOLE-MILK \r\nSafe:yes;Accept-Additions:WHOLE-MILK;\r\ntest" :: String)
    describe "PotErrorResponse" $
        it "builds a Coffee Error Response" $
            show NA `shouldBe` ("406 Not Acceptable" :: String)
