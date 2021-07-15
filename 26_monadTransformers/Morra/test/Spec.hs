import Test.Hspec
import Morra


main :: IO ()
main = hspec $ do
    describe "handlePlayerInput" $ do
        it "Returns MorraQuit on \"quit\"" $ do
            m <- handlePlayerInput "" (0,0) "quit" 
            m `shouldBe` MorraQuit
        it "Returns MorraBound on erroneous string input" $ do
            m <- handlePlayerInput "" (0,0) "123abc"
            m `shouldBe` MorraBound
        it "Returns MorraBound when input is out of bounds" $ do
            m <- handlePlayerInput "" (0,10) "-10"
            m `shouldBe` MorraBound
        it "Returns MorraInt on proper input" $ do 
            m <- handlePlayerInput "" (0,5) "04"
            m `shouldBe` MorraInt 4