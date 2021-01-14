{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DOTparser where

import Control.Applicative
import Control.Monad (void)

import Data.Char (ord)
import Data.ByteString (ByteString)
import Data.Graph.Inductive.Graph

import Text.RawString.QQ
import Text.Trifecta

import Test.Hspec

-- datatypes (Lifted from fgl documentation)
-- | Unlabeled node
--    type  Node   = Int
-- | Labeled node
--    type LNode a = (Node,a)
--
-- | Unlabeled edge
--    type  Edge   = (Node,Node)
-- | Labeled edge
--    type LEdge b = (Node,Node,b)
--
-- | Minimum implementation: 'empty', 'isEmpty', 'match', 'mkGraph', 'labNodes'
--    class Graph gr where
--      {-# MINIMAL empty, isEmpty, match, mkGraph, labNodes #-}
--
--      -- | An empty 'Graph'.
--      empty     :: gr a b
--    
--      -- | True if the given 'Graph' is empty.
--      isEmpty   :: gr a b -> Bool
--
--      -- | Decompose a 'Graph' into the 'MContext' found for the 
--           given node and the remaining 'Graph'.
--      match     :: Node -> gr a b -> Decomp gr a b
--
--      -- | Create a 'Graph' from the list of 'LNode's and 'LEdge's.
--      --
--      --   For graphs that are also instances of 'DynGraph', @mkGraph ns
--      --   es@ should be equivalent to @('insEdges' es . 'insNodes' ns)
--      --   'empty'@.
--      mkGraph   :: [LNode a] -> [LEdge b] -> gr a b
--
--      -- | A list of all 'LNode's in the 'Graph'.
--      labNodes  :: gr a b -> [LNode a]


dotEx :: ByteString
dotEx = [r|
digraph A {

  A -> B

}
|]

dotEx' :: ByteString
dotEx' = [r|
digraph D {

  A -> {B, C, D} -> {F}

}
|]


nodeEx :: String 
nodeEx = "A -> B -> C"

goToNode :: Parser ()
goToNode = spaces >> void (string "->")

parseNodeToken :: Parser Char 
parseNodeToken = spaces *> alphaNum <* (goToNode <|> eof)

genNode :: Parser Node 
genNode = 
  ord <$> parseNodeToken

genLNode :: Parser (LNode String)
genLNode = do
  a <- parseNodeToken
  return (ord a, [a])


genEdge :: Parser Edge
genEdge = (,) <$> genNode <*> genNode

genLEdge :: Parser (LEdge String)
genLEdge = do
  (a, aName) <- genLNode 
  (b, bName) <- genLNode
  return (a, b, aName <> " to " <> bName)




-- Test-suite
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

test :: IO ()
test = hspec $ do
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

