module ValidationApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data V' e a =
    Foil e
  | Socc a
  deriving (Eq, Show)

-- same as Either
instance Functor (V' e) where
  fmap _ (Foil x) = Foil x
  fmap f (Socc a) = Socc $ f a

-- This is different
instance Monoid e => Applicative (V' e) where
  pure = Socc
-- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) (Foil x) (Foil y) = Foil $ x <> y
  (<*>) (Foil x) _ = Foil x
  (<*>) _ (Foil y) = Foil y
  (<*>) (Socc x) (Socc y) = Socc $ x y

instance (Arbitrary e, Arbitrary a) => Arbitrary (V' e a) where
  arbitrary = oneof [ Foil <$> arbitrary 
                    , Socc <$> arbitrary]

instance (Eq e, Eq a) => EqProp (V' e a) where
  (=-=) = eq

-- Tests
data Errors = 
    DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

type VEI = V' [Errors] Integer

vprimeReplicate = do
  print $ success == Socc 2
  print $ failure == Foil [StackOverflow]
  print $ failure' == Foil [StackOverflow]
  print $ failures == Foil [StackOverflow, MooglesChewedWires]
    where success = Socc (+1) <*> Socc 1 :: VEI
          failure = Socc (+1) <*> Foil [StackOverflow] :: VEI
          failure' = Foil [StackOverflow] <*> Socc (+1) :: VEI
          failures = Foil [StackOverflow] 
                   <*> Foil [MooglesChewedWires] :: VEI


type VSSSI = V' String (String, String, Integer)

batchTest :: IO ()
batchTest = do 
  quickBatch $ functor (trigger :: VSSSI)
  quickBatch $ applicative (trigger :: VSSSI)
    where trigger = undefined
