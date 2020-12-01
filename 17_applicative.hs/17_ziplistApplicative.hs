module ZiplistApplicative where

import Control.Applicative
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Datatype definition

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  (=-=) xs ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs 
                in take 3000 l
          ys' = let (ZipList' l) = ys
                in take 3000 l

instance Semigroup a => Semigroup (ZipList' a) where
  (<>) (ZipList' []) x = x
  (<>) x (ZipList' []) = x
  (<>) (ZipList' as) (ZipList' xs) = ZipList' $ (<>) <$> as <*> xs 

instance Monoid a => Monoid (ZipList' a) where
  mempty = ZipList' []
  mappend = liftA2 mappend 

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs


-- This doesn't pass the tests.... need a second pair of eyes
instance Applicative ZipList' where
  pure x = ZipList' [x]
  (<*>) (ZipList' []) _ = ZipList' []
  (<*>) _ (ZipList' []) = ZipList' []  
  (<*>) (ZipList' (f:fs)) (ZipList' (x:xs)) = ZipList' (y:ys) 
    where y = f x
          ZipList' ys = ZipList' fs <*> ZipList' xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary 


-- type aliases for test
type ZipListSemi = (ZipList' String, Integer)
type ZipListSSI = ZipList' (String, String, Integer)

specTest :: IO ()
specTest = hspec $ do
  describe "Applicative" $ do
    it "Identity" $ do
      pure id  <*> z' `shouldBe` z'
    it "3 functions, 1 value" $ do
      z <*> pure 1 `shouldBe` zl' [10, 2, 9]
    it "1 function, 3 values" $ do
      pure (+1) <*> z'' `shouldBe` zl' [2, 3, 4]
    it "3 functions, 3 values" $ do
      z <*> z'' `shouldBe` zl' [10, 4, 11]
    where zl' = ZipList'
          z = zl' [(+9), (*2), (+8)]
          z' = zl' [1,2] 
          z'' = zl' [1..3]

main :: IO () 
main = do
  quickBatch $ semigroup (trigger :: ZipListSemi)
  quickBatch $ functor (trigger :: ZipListSSI)
  quickBatch $ applicative (trigger :: ZipListSSI)
    where trigger = undefined
