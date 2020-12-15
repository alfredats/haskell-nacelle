module Tree21 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative

data Tree a =
    Empty 
  | Leaf a 
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where 
  fmap _ Empty        = Empty
  fmap f (Leaf x)     = Leaf $ f x
  fmap f (Node x y z) = Node (fmap f x) (f y) (fmap f z)

instance Foldable Tree where
  foldMap _ Empty = mempty 
  foldMap f (Leaf x) = f x
  foldMap f (Node x y z) = foldMap f x <> f y <> foldMap f z

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node x y z) = 
    liftA3 Node (traverse f x) (f y) (traverse f z)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof [ return Empty
                    , Leaf <$> arbitrary
                    , liftA3 Node arbitrary arbitrary arbitrary]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

treeSpec = do
  let trig :: Tree (Int, Int, [Int])
      trig = undefined
  quickBatch $ functor trig
  quickBatch $ traversable trig
