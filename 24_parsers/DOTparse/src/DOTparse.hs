{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DOTparse where

import Control.Applicative
import Control.Monad (void)

import Data.Char (ord)
import Data.ByteString (ByteString)
import Data.Graph.Inductive.Graph
import Data.Tuple.Extra

import Text.RawString.QQ
import Text.Trifecta

import Test.Hspec

someFunc :: IO ()
someFunc = putStrLn "someFunc"


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


genNode :: Parser Node 
genNode = 
  ord <$> alphaNum

genLNode :: Parser (LNode String)
genLNode = do
  a <- alphaNum
  return (ord a, [a])

parseEdgeStatement :: Parser ([LNode String], [LEdge String])
parseEdgeStatement = do
  ns <- some $ spaces *> genLNode <* (spaces >> optional (string "->"))
  let f :: [LEdge String] -> (Node, String) -> [LEdge String]
      f x y
        | null x = [(fst y, fst y, snd y)]
        | otherwise = 
            let x' = last x in 
              x <> [(snd3 x', fst y, [last $ thd3 x'] <> snd y)]
      edges = foldl f [] ns
  return (ns, tail edges)



genEdge :: Parser Edge
genEdge = (,) <$> genNode <*> genNode

genLEdge :: Parser (LEdge String)
genLEdge = do
  (a, aName) <- genLNode 
  (b, bName) <- genLNode
  return (a, b, aName <> " to " <> bName)


--      -- | Create a 'Graph' from the list of 'LNode's and 'LEdge's.
--      --
--      --   For graphs that are also instances of 'DynGraph', @mkGraph ns
--      --   es@ should be equivalent to @('insEdges' es . 'insNodes' ns)
--      --   'empty'@.
--      mkGraph   :: [LNode a] -> [LEdge b] -> gr a b

genGraph :: Parser (gr String String)
genGraph = do
  spaces
  _ <- token (string "strict") <?> "strict"
  _ <- (string "graph" <|> string "digraph") >> spaces
  _ <- skipMany alphaNum >> spaces
  a <- braces (many parseEdgeStatement)
  let nodes :: [LNode String]
      nodes = mconcat $ fst a 
  return a

graphEx :: String
graphEx = [r|
strict digraph myGraph { A -> B }
|]

nodeEx :: String 
nodeEx = "A -> B -> C"


-- UTILITY FUNCTIONS
vt p = spaces >> void <$> token p



-- Test-suite
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

test :: IO ()
test = hspec $ do
  let mkLNode :: (Int, String) -> LNode String
      mkLNode = id
      mkLEdge :: (Int, Int, String) -> LEdge String
      mkLEdge = id
  describe "Node generator" $ do
    it "Given arrow notation, generates the nodes required" $ do
      let p = spaces *> genNode <* (spaces >> optional (string "->")) 
          m = parseString (some p) mempty nodeEx 
          r = maybeSuccess m
      r `shouldBe` Just [65, 66, 67] 
  describe "Labelled Node generator" $ do
    it "Given arrow notation, generates the\
       \ labelled nodes required" $ do
      let m = parseString (parseEdgeStatement <* eof) mempty nodeEx
          r = maybeSuccess m
          expOut1 = map mkLNode [ (65, "A")
                                , (66, "B")
                                , (67, "C")
                                ]
          expOut2 = map mkLEdge [ (65, 66, "AB")
                                , (66, 67, "BC")
                                ]
      r `shouldBe` Just (expOut1, expOut2)

