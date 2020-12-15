module Chapter21 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative

-- Traversable
--
--  class (Functor t, Foldable t) => Traversable t where
--    {-# MINIMAL traverse | sequenceA #-}
--    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--    traverse f = sequenceA . fmap f
--    sequence A :: Applicative f => t (f a) -> f (t a)
--    sequenceA = traverse id
--  
--  Traversable allows us to transform values inside a structure like a
--  Functor, producing Applicative effects along the way, and lift 
--  the Applicative effects outside of the Traversable structure.
--
--  The `traverse` function maps each element of a structure to an 
--  action, evalates the actions from left to right, and flips the 
--  flips the traversable structure on the outside for the Applicative
--  structure that was introduced by the action.
--  
--  `sequenceA` is slightly simpler. It just flips two layers of 
--  structures around.
--
--  An example:
--    
--    -- recall the type sigs of fmap and traverse
--    fmap :: Functor f => (a -> b) -> f a -> f b
--    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--    
--    -- suppose we had the following functions with these type sigs
--    myData :: [String]
--    myFunc :: String -> IO Record
--
--    -- actual funcs
--    wrong :: [IO Record]  -- unless we want a list of IO actions?
--    wrong = fmap myFunc myData
--
--    right :: IO [Record] 
--    right = traverse myFunc myData
--
--
--
--  sequenceA
--
--  This will just be a compilation of examples. They're pretty 
--  self-explantory.
--
--    xs  = [Just 1, Just 2, Just 3]
--    xsn = [Just 1, Just 2, Nothing]
--
--    sequenceA $ Just <$> [1,2,3] = 
--      Just [1,2,3]
--
--    sequenceA xs =
--      Just [1,2,3]
--    sequenceA xsn =
--      Nothing
--
--    fmap sum $ sequenceA xs = 
--      Just 6
--    fmap product $ sequenceA xsn = 
--      Nothing
-- 
--  It should be noted that the Data.Maybe module has a function called 
--  `catMaybes` that does what sequenceA does, but ignores Nothing
--  values. 
--
--    catMaybes xsn = [1,2] 
--    sum $ catMaybes xsn = 6
--
--
--
--  traverse
--
--    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--    traverse f = sequenceA . fmap f
--
--  The type signatures of traverse, fmap and (=<<), on inspecction, are
--  very similar. This is because they all perform similar actions, 
--  mapping a function over values embedded in structure. The only 
--  difference is how they handle structure. traverse, particularly, 
--  flips the layers of structure, as seen in its type sig.
--  
--
--  mapM is traverse
--
--    mapM :: Monad m => (a -> m b) -> [a] -> m [b]
--
--  The traverse function is actually a generalization of mapM. It is 
--  easy to see why by abstracting the [] structure in mapM to any 
--  traversable data structure, and also loosening the Monad requirement
--  to only require Applciative.
--
--
--
--  So, what's Traversable for?
--
--  Simply, Traversables are useful for situations where two type
--  constructors need to be flipped (traverse if u need to perform a 
--  mapping, sequenceA if you don't). 
--
--
--
--  Traversable instances
--
--  Either
data Either' a b = 
    Left' a
  | Right' b
  deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Left' x) = Left' x
  fmap f (Right' y) = Right' $ f y

instance Applicative (Either' e) where
  pure            = Right'
  Left' e <*> _   = Left' e
  Right' f <*> r  = f <$> r

instance Foldable (Either' a) where
  foldMap _ (Left' _)   = mempty
  foldMap f (Right' y)  = f y
  foldr _ z (Left' _)   = z
  foldr f z (Right' y)  = f y z

instance Traversable (Either' a) where
  traverse _ (Left' x) = pure (Left' x)
  traverse f (Right' y) = Right' <$> f y

--
--
-- Traversable Laws
--
--  Naturality
--  
--    t . traverse f = traverse (t . f)
--
--    Since a traversed function f generates the structure that appears
--    on the "outside" of the traverse operation, there is no reason why
--    we can't incorporate the function into the traversal itself.
--
--
--  Identity
--    
--    traverse Identity = Identity
--
--    Traversing a data constructor over a value produces the same 
--    result as putting the value in the data constructor. It also 
--    implies that Traversable instances should not add or inject 
--    new structure or effects.
--
--
--  Composition 
--    
--    traverse (Compose . fmap g . f) =
--      Compose . fmap (traverse g) . traverse f
--
--    --------------------------------------------
--    traverse (Compose . fmap g . f) =
--      Compose . (traverse (fmap g . f)) =           [Naturality law]
--      Compose . (someFunc . (traverse f)) =         [Naturality law]
--      Compose . (fmap (traverse g) . traverse f)    
--                 ^(***)
--    --------------------------------------------
--    (***):
--      let f :: Int -> [String]
--          g :: String -> Maybe Integer
--      then,
--          traverse (fmap g . f) :: Traversable t =>
--            t Int -> IO (t (Maybe Integer))
--          traverse f :: Traversable t =>
--            t Int -> IO (t String)
--          someFunc necessarily needs to have input type
--            IO (t String), and output type IO (t (Maybe Integer))
--          with fmap :: (a -> b) -> f a -> f b concretized,
--            fmap :: (SOME TYPE SIG) -> 
--                    IO (t String) -> 
--                    IO (Maybe (t Integer))
--    --------------------------------------------
--    
--    There are a similar set of laws for sequenceA.
--
--      Naturality
--        
--        t . sequenceA = sequenceA . fmap t
--
--
--      Identity 
--        
--        sequenceA . fmap Identity = Identity
--
--
--      Composition 
--
--        sequenceA . fmap Compose = 
--          Compose . fmap sequenceA . sequenceA
--
--  
--  checkers
--    
--  chackers has the laws for traversable. 
--
--  type TI = []
--  
--  main = do
--    let trigger :: TI (Int, Int, [Int])
--        trigger = undefined
--    quickBatch (traversable trigger)



-- Chapter Exercises:
-- 
--  Write a Traversable instance for the datatype provided, filling
--  in any required superclasses. Use QuickCheck to validate your
--  instances.
--
-- 1)
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where 
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq


specIdentity = do
    let trigger :: Identity (Int, Int, [Int])
        trigger = undefined
    quickBatch (traversable trigger)


-- 2)
newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty 

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure (Constant x)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary 

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

specConstant = do
    let trigger :: Constant Int (Int, Int, [Int]) 
        trigger = undefined
    quickBatch (traversable trigger)

-- 3)
data Optional a = 
    Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [ return Nada
                    , Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

specOptional = do
    let trigger :: Optional (Int, Int, [Int]) 
        trigger = undefined
    quickBatch (traversable trigger)


-- 4) 
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [ return Nil
                    , Cons <$> arbitrary <*> arbitrary ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

specList = do 
  let trigger :: List (Int, Int, [Int])
      trigger = undefined
  quickBatch $ traversable trigger


-- 5)
data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) 
  => Arbitrary (Three a b c) where
    arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

specThree = do 
  let trigger :: Three Int Int (Int, Int, [Int])
      trigger = undefined
  quickBatch $ traversable trigger

-- 6)
-- This one is essentially the same as (5).

-- 7)
data Big a b = Big a b b deriving (Eq, Ord, Show)

instance Functor (Big a)where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
  foldMap f (Big _ y z) = f y <> f z

instance Traversable (Big a) where
  traverse f (Big x y z) = Big x <$> f y <*> f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

specBig = do
  let trigger :: Big Int (Int, Int, [Int])
      trigger = undefined
  quickBatch $ traversable trigger

-- 8)
-- Same as (7). use liftA4 for traversable instance.



-- S
-- refer to 21_S.hs






