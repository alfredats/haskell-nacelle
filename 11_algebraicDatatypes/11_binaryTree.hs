module BinaryTree where


data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

insert' :: Ord a
        => a
        -> BinaryTree a
        -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)


mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)


testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 
                 1
                 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf)
                   2
                   (Node Leaf 5 Leaf)

mapOkay = 
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"


-- conversion of binary trees into lists
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node lft x rght) = [x] ++ (preorder lft) ++ (preorder rght)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node lft x rght) = (inorder lft) ++ [x] ++ (inorder rght)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node lft x rght) = (postorder lft) ++ (postorder rght) ++ [x]


--   2
--  /  \
-- 1    3
--
-- 1 : 2 : 3 

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = 
  f a (foldTree f (foldTree f b right) left)

-- 
--   2
--  /  \
-- 1    3
--
-- foldTree testTree b = 
-- f 2 (foldTree f (foldTree b (N L 3 L) (N L 1 L)) = 
-- f 2 (foldTree f (
--                    f 3 (foldTree f (foldTree b L) L) 
--                 ) (N L 1 L) =  
-- f 2 (foldTree f ( 
--                    f 3 (foldTree f b L )
--                 ) (N L 1 L) =
-- f 2 (foldTree f b' (N L 1 L) =                           [b' = f 3 b]
-- f 2 (f 1 (foldTree f (foldTree b' L) L) =  
-- f 2 (f 1 (foldTree f b' L) =
-- f 2 (f 1 b') =
-- f 2 (f 1 (f 3 b)  //  Q.E.D
--
-- some insights:
--    1. from the perspective of the "3" node, the parent nodes and 
--       corresponding subnodes to the left of "3" serve as the "b"
--       parameter in foldTree f b (Node Leaf 3 Leaf)



testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf)
                2
                (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
               then putStrLn "Preorder okay!"
               else putStrLn "Preorder not okay!"

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3]
              then putStrLn "Inorder fine!"
              else putStrLn "Inorder no okay"

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2]
                then putStrLn "Postorder fine!"
                else putStrLn "Postorder no no"

main :: IO ()
main = do 
  testPreorder
  testInorder
  testPostorder


