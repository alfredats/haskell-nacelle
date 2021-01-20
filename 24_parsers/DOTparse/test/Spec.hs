import DOTparse

import Text.Trifecta
import Data.Graph.Inductive.Graph
import Test.Hspec


main :: IO ()
main = hspec $ do
  let mkLNode :: (Int, String) -> LNode String
      mkLNode = id
  describe "Node generator" $ do
    it "Given arrow notation, generates the nodes required" $ do
      let m = parseString (some genNode) mempty nodeEx 
          r = maybeSuccess m
      r `shouldBe` Just [65, 66, 67] 
  describe "Labelled Node generator" $ do
    it "Given arrow notation, generates the\
      \ labelled nodes required" $ do
      let m = parseString (some genLNode) mempty nodeEx
          r = maybeSuccess m
          expOut = map mkLNode [ (65, "A")
                               , (66, "B")
                               , (67, "C")
                               ]
      r `shouldBe` Just expOut

