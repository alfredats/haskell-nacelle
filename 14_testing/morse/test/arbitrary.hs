module Main where

-- Using QuickCheck well requires writing instances of the Arbitrary type
-- for our data types.

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof) -- for use in sum Gens

-- Trivial Arbitrary
data Trivial = Trivial deriving (Eq, Show) -- datatype to write arbitrary instance of

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen


-- Identity
data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do        -- Gen is used to generate a single value of type
  a <- arbitrary        -- a, and returned within the structure of
  return (Identity a)   -- Identity

instance Arbitrary a =>
         Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen


-- Arbitrary Products 
data Pair a b = 
  Pair a b
  deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) =>
           Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen


-- Arbitrary Sums
data Sum a b = First a
             | Second b
             deriving (Eq, Show)

sumGenEqual :: (Arbitrary a,
                Arbitrary b) =>
               Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a,
         return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual




main :: IO ()
main = do 
  sample trivialGen

