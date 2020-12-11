{-# LANGUAGE FlexibleContexts #-}
module Chapter20 where

import Data.Monoid 
import Data.Foldable
import Test.Hspec
import Control.Applicative

-- Foldable
--
--  The idea of a Foldable typeclass is to have a "class 
--  of data structures that can be folded to a summary 
--  value". We've seen this happen already, with foldr &
--  foldl, but there are many other operations within this
--  typeclass.
--
--  The definition of the typeclass is as follows:
--
--    class Foldable (t :: * -> *) where
--      {-# MINIMAL foldMap | foldr #-}
--
--  As seen in the line after the typeclass declaration,
--  the minimal complete definition of the typeclass for
--  a given datatype must contain either foldMap or foldr.
--
-- 



-- Revenge of the Monoids
--
--  This was not explicitly mentioned previously, but a 
--  fold necessarily implies the existence of a binary 
--  associative operation that has an identit value.
--
--  We see this in both the definitions belonging to
--  the first two operations within Foldable.
--
--    class Foldable (t :: * -> *)
--      {-# MINIMAL foldMap | foldr #-}
--      fold :: Monoid m => t m -> m
--      foldMap :: Monoid m => (a -> m) -> t a -> m
--
--  'fold' allows the combination of elements inside a 
--  Foldable structure with the Monoid instance defined
--  for the elements, while 'foldMap' first maps each 
--  element of the structure to a Monoid then combines 
--  the results using the specified instance of Monoid.
--
--  'foldMap' is slightly different. It takes a function
--  as its first argument, and that function must
--  explicitly map each element of a foldable structure
--  to a Monoid.
--
--    foldMap Sum [1,2,3,4] = Sum {getSum = 10}
--    foldMap All [True, False] = All {getAll = False}
--    foldMap First [Just 1, Nothing, Just 5] 
--      = First {getFirst = Just 1}
--    foldMap Last [Just 1, Nothing, Just 5] 
--      = First {getFirst = Just 5}
--
--  The idea is that it can be used to transform the 
--  values within the foldable structure using the 
--  function, and the monoidal mappend rules of the 
--  Monoid used will handle the rest.
--
--    foldMap First [Just 1, Nothing, Just 5] 
--      = [First {getFirst = Just 1}
--
--  Given a foldable structure containing monoidal 
--  values already however, the input function can instead
--  be used to transform the values before they are 
--  monoidally joined.
--
--    let xs = map Product [1..3],
--    foldMap (*5) xs = Product {getProduct = 1*5}
--                      <> Product {getProduct = 2*5}
--                      <> Product {getProduct = 3*5}
--                    = Product {getProduct = 750}
--
--
--  It should be noted that the 'foldr' function ignores
--  any monoidal structure within its input, and uses 
--  the input function as the monoidal join.
--
--    let xs = map Sum [2..4]
--        ys = map Product [2..4]
--    foldr (*) 3 xs = Sum {getSum = 72}
--    foldr (*) 3 ys = Product {getProduct = 72}
--
--  Simply put, Foldable is a generalization of a 
--  catamorphism to different datatypes. 



-- Demonstrating Foldable Instances
--
--  Identity
data Identity a = Identity a deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x
  
--  Remember that foldables are meant to be catamorphisms.
--  In this case, we are "consuming" the Identity data 
--  constructor.


--  Maybe (implemented as Optional)
data Optional a = 
    Nada
  | Yep a
  deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z
  
  foldl _ z Nada = z
  foldl f z (Yep x) = f z x
  
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

--  With 'foldMap' and Nada, GHC will throw a fuss if 
--  the an output monoidal type is not declared.



-- Some basic derived operations
-- 
--  These functions are generalizations of ones we've 
--  seen previously that were applicable to lists.
--
--    toList :: t a -> [a]
--    null :: t a -> Bool
--    length t a -> Int
--    elem :: Eq a => a -> t a -> Bool
--    maximum :: Ord a => t a -> a
--    minimum :: Ord a => t a -> a
--    sum :: (Foldable t, Num a) => t a -> a
--    product :: (Foldable t, Num a) => t a -> a



-- Exercises: Library functions
--
--  Implement the functions in terms of foldMap or foldr
--  from Foldable, then try them out with multiple types
--  that have Foldable instances.

-- 1) 
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

specSum' :: IO ()
specSum' = hspec $ do
  describe "sum'" $ do
    it "sum' [1, 2, 3] == 6" $ do
      sum' [1,2,3] `shouldBe` 6
    it "sum' [1] == 1" $ do
      sum' [1] `shouldBe` 1
    it "fmap sum' (Just [1,2,3,4,5]) == Just 15" $ do
      sum' <$> Just [1,2,3,4,5] `shouldBe` Just 15

-- 2)
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

specProduct' :: IO ()
specProduct' = hspec $ do
  describe "product'" $ do
    it "product' [1,2,3] == 6" $ do
      product' [1,2,3] `shouldBe` 6
    it "product' [1] == 1" $ do
      product' [1] `shouldBe` 1
    it "fmap product' (Just [1,2,3,4,5]) == Just 120" $ do
      product' <$> Just [1,2,3,4,5] `shouldBe` Just 120

-- 3)
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (\y -> Any $ x == y)

specElem' :: IO ()
specElem' = hspec $ do
  describe "elem'" $ do
    it "elem' 1 [1,2,3] == True" $ do
      elem' 1 [1,2,3] `shouldBe` True
    it "elem' 0 [1,2,3] == False" $ do
      elem' 0 [1,2,3] `shouldBe` False
    it "elem' 1 (Just 1) == True" $ do
      elem' 1 (Just 1) `shouldBe` True
    it "fmap (elem' 1) (Just [1,2,3]) == Just True" $ do
      elem' 1 <$> Just [1,2,3] `shouldBe` Just True

-- 4)
min' :: (Foldable t, Ord a) => t a -> Maybe a
min' = getAlt . foldMap (Alt . Just)

specMin' :: IO ()
specMin' = hspec $ do
  describe "min'" $ do
    it "min' [1,2,3,4,5] == Just 1" $ do
      min' [1,2,3,4,5] `shouldBe` Just 1
    it "min' [] == Nothing" $ do
      min' ([] :: [Integer]) `shouldBe` Nothing
    it "min' <$> (Just [1,2,3,4,5]) == Just (Just 1)" $ do
      min' <$> Just [1,2,3,4,5] `shouldBe` Just (Just 1)

-- 5)
max' :: (Foldable t, Ord a) => t a -> Maybe a
max' = foldr (maxcmp . Just) Nothing
        where maxcmp a b = case compare a b of 
                              GT -> a
                              _  -> b

specMax' :: IO ()
specMax' = hspec $ do
  describe "max'" $ do
    it "max' [1,2,3,5,4] = Just 5" $ do
      max' [1,2,3,4,5] `shouldBe` Just 5
    it "max' [] == Nothing" $ do
      max' ([] :: [Int]) `shouldBe` Nothing
    it "max' <$> (Just [1,2,3,4,5]) == Just (Just 5)" $ do
      max' <$> Just [1,2,3,4,5] `shouldBe` Just (Just 5)

-- 6)
null' :: (Foldable t) => t a -> Bool
null' = foldr ((&&) . const False) True 

specNull' :: IO ()
specNull' = hspec $ do
  describe "null'" $ do
    it "null' [] == True" $ do
      null' [] `shouldBe` True
    it "null' [1] == False" $ do
      null' [1] `shouldBe` False
    it "null' Nothing == True" $ do
      null' Nothing `shouldBe` True
    it "null' (Just 1) == False" $ do
      null' (Just 1) `shouldBe` False

-- 7)
len' :: (Foldable t) => t a -> Int
len' = foldr ((+) . const 1) (0 :: Int)

specLen' :: IO ()
specLen' = hspec $ do
  describe "len'" $ do
    it "len' [] == 0" $ do
      len' [] `shouldBe` 0
    it "len' [1] == 1" $ do
      len' [1] `shouldBe` 1
    it "len' Nothing == 0" $ do
      len' Nothing `shouldBe` 0
    it "len' (Just 1) == 1" $ do
      len' (Just 1) `shouldBe` 1
    it "len' (Just [1,2,3,4]) == 1" $ do
      len' (Just [1,2,3,4]) `shouldBe` 1

-- 8)
toList' :: Foldable t => t a -> [a]
toList' = foldr (:) [] 

specToList' :: IO ()
specToList' = hspec $ do
  describe "toList'" $ do
    it "toList' (Just 1) == [1]" $ do
      toList' (Just 1) `shouldBe` [1]
    it "toList' (1,2) == [2]" $ do
      toList' (1,2) `shouldBe` [2]
    it "concatMap toList' [Just 1, Just 2, Nothing] == [1.2]" $ do
      concatMap toList' [Just 1, Just 2, Nothing] `shouldBe` [1,2]

-- 9)
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

specFold' :: IO ()
specFold' = hspec $ do
  describe "fold'" $ do
    it "fold' [Sum 1, Sum 2, Sum 3] == Sum 6" $ do
      fold' (map Sum [1,2,3]) `shouldBe` Sum 6
    it "fold' [Any True, Any False] == Any True" $ do
      fold' (map Any [True, False]) `shouldBe` Any True
    it "fold' [All True, All False] == All False" $ do
      fold' (map All [True, False]) `shouldBe` All False

-- 10)
foldM' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldM' f = foldr ((<>) . f) mempty

specFoldM' :: IO ()
specFoldM' = hspec $ do
  describe "foldM'" $ do
    it "foldM' Sum [1,2,3] == Sum 6" $ do
      foldM' Sum [1,2,3] `shouldBe` Sum 6
    it "foldM' Product [1,2,3] == Product 6" $ do
      foldM' Product [1,2,3] `shouldBe` Product 6
    it "foldM' Any [True, False] == Any True" $ do
      foldM' Any [True, False] `shouldBe` Any True

specLibFun :: IO ()
specLibFun = do
  specSum'
  specProduct'
  specElem'
  specMin'
  specMax'
  specNull'
  specLen'
  specToList'
  specFold'
  specFoldM'



-- Chapter Exercises
--  Write Foldable instances for the following datatypes

--  1)
data Constant a b = Constant b deriving (Eq, Show)

-- I know i can do either one, but i wna try doing both
instance Foldable (Constant a) where
  foldr f z (Constant x) = f x z
  foldMap f (Constant b) = f b

--  2)
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where 
  foldMap f (Two _ x) = f x
  foldr f z (Two _ x) = f x z

--  3)
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c
  foldr f z (Three _ _ c) = f c z

--  4)
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ x y) = f x <> f y
  foldr f z (Three' _ x y) = f x $ f y z 

--  5)
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ x y z) = f x <> f y <> f z
  foldr f a (Four' _ x y z) = f x . f y $ f z a


-- Thinking cap time. Write a filter function for 
-- Foldable types using the foldMap function.
filterF :: (Applicative f, Foldable t, Monoid (f a))
           => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

-- This function requires an explicit declaration of 
-- output type.
--
-- Prelude> xs = filterF (== 1) [1,2,1,4,5]
-- Prelude> show (xs :: [Integer])
-- "[1,1]



-- g :: a -> Bool
-- xs :: t a
-- ys :: f a
-- foldMap :: (a -> m) -> t a -> m
--
-- we need 
--  foldMap :: (a -> f a) -> t a -> f a
--  g' :: a -> f a
--  (f a) as a monoid that has monoidal join which 
--  ignores mempty
--
-- let xs = [1, 2, 1, 4, 5]
--    , f = (==) 1
--
-- expected output of filterF f xs = [1, 1]
--
-- 
