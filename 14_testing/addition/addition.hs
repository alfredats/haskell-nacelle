module Addition where

import Test.QuickCheck
import Test.Hspec


dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
  where go n d count 
          | n < d = (count, n)
          | otherwise =
              go (n - d) d (count + 1)

-- Intermission exercise
--    write a function that multiplies two numbers using recursive 
--    sumnation, then write hspec tests for it.
myMult :: (Eq a, Num a) => a -> a -> a
myMult num mult = go num mult 0
  where 
    go n m acc
      | m == 0 = acc
      | otherwise = go n (m-1) (acc+num)

-- Writing Tests
main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 rem 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
    it "x + 1 is always greater than x" $ do
      property $ \x -> (x :: Integer) + 1 > x 
  describe "Multiplication" $ do
    it "3 mulitplied by 2 is 6" $ do
      myMult 3 2 `shouldBe` 6
    it "9 multiplied by 0 is 0" $ do
      myMult 9 0 `shouldBe` 0 



-- Generators
genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a,b,c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a,b,c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)  -- frequency allows you to specify a
            , (3, return (Just a))] -- "probability distribution" 


-- Using the generators
type G1 = Gen (Int, Float)
showG1 :: IO ()
showG1 = do
  sample (genTuple :: G1)

type G2 = Gen ([()], Char)
showG2 :: IO ()
showG2 = do
  sample (genTuple :: G2)



