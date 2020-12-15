module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a)
        => Arbitrary (S n a) where
          arbitrary = S <$> arbitrary <*> arbitrary


instance ( Applicative n
         , Testable (n Property)
         , Eq a 
         , Eq (n a)
         , EqProp a)
        => EqProp (S n a) where
      (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a) 

instance Foldable (S n) where
  foldMap f (S _ a) = f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

main = sample' (arbitrary :: Gen (S [] Int))

specS = do 
  let trigger :: S [] (Int, Int, [Int])
      trigger = undefined
  quickBatch $ traversable trigger
