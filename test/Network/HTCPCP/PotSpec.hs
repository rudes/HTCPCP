module Network.HTCPCP.PotSpec where

import Test.Hspec
import Network.HTCPCP.Pot

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "PotURI" $ do
        it "builds a Coffee URI" $ do
            show PotURI { potUriScheme = "coffee"
                   , potUriHost = "host"
                   , potDesignator = 2
                        , potAdditions = MILK WHOLEMILK
                    } `shouldBe` ("coffee://host/pot-2#WHOLE-MILK" :: String)
