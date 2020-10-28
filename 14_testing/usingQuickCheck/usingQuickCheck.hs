module UsingQuickCheck where

import Test.Hspec 
import Test.QuickCheck
import Data.Tuple.Extra (fst3, snd3, thd3)

-- Generators
integerGen :: Gen Integer
integerGen = arbitrary

doubleGen :: Gen Double
doubleGen = arbitrary

charGen :: Gen Char
charGen = arbitrary

integerListGen :: Gen [Integer]
integerListGen = listOf (integerGen)

doubleListGen :: Gen [Double]
doubleListGen = listOf (doubleGen)

stringGen :: Gen [Char]
stringGen = listOf (charGen)

tupleGen :: (Arbitrary a) => Gen (a,a)
tupleGen = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)

threepleGen :: (Arbitrary a) => Gen (a, a, a)
threepleGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

-- my list generators 
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, _)  = (Just y, x >= y)

listGen :: Arbitrary a => Gen [a] -- Generalized list generator
listGen = sized $ \n ->
  frequency
    [ (1, return [])
    , (n, (:) <$> arbitrary <*> listGen)
    ]

intListGen :: Gen [Integer] -- Integer List Generator
intListGen = listGen


-- Exercises 
-- 1)
half :: (Fractional a) => a -> a
half x = x / 2

halfIdentity :: (Fractional a) => a -> a
halfIdentity = (*2) . half 

prop_halfIdentity :: Property
prop_halfIdentity = 
  forAll doubleGen (\c -> (halfIdentity c) == c)



-- 2)
intListGen' :: Gen [Integer] -- Integer List Generator using list generator methods in QuickCheck
intListGen' = listOf (integerGen)

listGen' :: (Enum a, Arbitrary a) => Gen [a] -- Generalized sorted list
listGen' = sized $ \n -> do                  -- generator
  a <- arbitrary
  return (take n . enumFrom $ a)

listGen'' :: (Ord a, Arbitrary a) => Gen (SortedList a) -- Using quickCheck modifiers
listGen'' = do 
  a <- arbitrary
  return a

prop_listOrdered_Integer :: Property 
prop_listOrdered_Integer = 
  forAll (listGen' :: Gen [Integer]) (\c -> listOrdered c == True)

prop_listOrdered_Float :: Property
prop_listOrdered_Float = 
  forAll (listGen' :: Gen [Float]) (\c -> listOrdered c == True)

prop_listOrdered_Char :: Property
prop_listOrdered_Char =
  forAll (listGen' :: Gen [Char]) (\c -> listOrdered c == True)

prop_listOrdered_Bool :: Property
prop_listOrdered_Bool = 
  forAll (listGen' :: Gen [Bool]) (\c -> listOrdered c == True)

prop_listOrdered_Integer' :: Property
prop_listOrdered_Integer' =
  forAll (listGen'' :: Gen (SortedList Integer)) (\c -> listOrdered (getSorted c) == True)

-- 3) Now, we’ll test the associative and commutative properties
--    of addition

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = 
  x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = 
  x + y == y + x

prop_plusAssociative :: Property
prop_plusAssociative = 
  forAll (threepleGen :: Gen (Int, Int, Int)) (\c -> plusAssociative (fst3 c) (snd3 c) (thd3 c) == True)

prop_plusCommutative :: Property
prop_plusCommutative =
  forAll (tupleGen :: Gen (Int, Int)) (\c -> plusCommutative (fst c) (snd c) == True)



-- 4) 
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y =
  x * y == y * x

multCommutative' :: (Eq a, Num a) => (a,a) -> Bool
multCommutative' (x,y) = 
  x * y == y * x

prop_multAssociative :: Property
prop_multAssociative =
  forAll (threepleGen :: Gen (Int, Int, Int)) (\c -> multAssociative (fst3 c) (snd3 c) (thd3 c) == True)

prop_multCommutative :: Property
prop_multCommutative =
  forAll (tupleGen :: Gen (Int, Int)) (\c -> multCommutative (fst c) (snd c) == True)



-- 5)
quotRemDecomp :: (Eq a, Integral a) => a -> a -> Bool
quotRemDecomp x y =
  (quot x y) * y + (rem x y) == x

divModDecomp :: (Eq a, Integral a) => a -> a -> Bool
divModDecomp x y =
  (div x y) * y + (mod x y) == x

prop_quotRemDecomp :: Property
prop_quotRemDecomp =
  forAll (tupleGen :: Gen (NonZero Int, NonZero Int)) (\c -> 
    quotRemDecomp (getNonZero . fst $ c) (getNonZero . snd $ c) == True)

prop_divModDecomp :: Property
prop_divModDecomp =
  forAll (tupleGen :: Gen (NonZero Int, NonZero Int)) (\(a,b) -> 
    divModDecomp (getNonZero a) (getNonZero b) == True)



-- 6)
expoAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
expoAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

prop_expoAssociative :: Property -- This should fail, but it only generates exceptions to the general rule
prop_expoAssociative =           -- that exponentiation is only right-associative
  forAll (threepleGen :: Gen (NonNegative Int, NonNegative Int, NonNegative Int)) (\(a,b,c) ->
    expoAssociative (getNonNegative a) (getNonNegative b) (getNonNegative c) == False)

expoCommutative :: (Eq a, Integral a) => a -> a -> Bool
expoCommutative x y =
  x ^ y == y ^ x

prop_expoCommutative :: Property -- As above, this should fail as it generates exceptions to the general 
prop_expoCommutative =           -- rule that exponentiation is not commutative
  forAll (tupleGen :: Gen (NonNegative Int, NonNegative Int)) (\(a,b) ->
    expoCommutative (getNonNegative a) (getNonNegative b) == False)



-- 7)
listReversal :: (Eq a) => [a] -> Bool
listReversal xs =
  id xs == reverse (reverse xs)

prop_listReversal :: (Eq a, Show a) => Gen [a] -> Property
prop_listReversal gen = 
  forAll gen (\c -> listReversal c == True) 



-- 8)
dolladolla :: (Eq b) => Fun a b -> a -> Bool
dolladolla (Fun _ f) xs =
  f xs == (f $ xs)

prop_dolladolla :: Property
prop_dolladolla = 
  property $ (dolladolla :: Fun String Integer -> String -> Bool) 

dotdot :: (Eq c) => Fun b c -> Fun a b -> a -> Bool
dotdot (Fun _ f) (Fun _ g) x =
  (f . g $ x) == f (g x)

prop_dotdot :: Property
prop_dotdot = 
  property $ (dotdot :: Fun Char Integer -> Fun String Char -> String -> Bool) 


-- I have to figure out how QuickCheck actually generates functions
myCoArb :: (CoArbitrary a) => Gen b -> a -> Gen b
myCoArb gen = flip coarbitrary gen 

nCoArb :: (Arbitrary a) => Gen a -> Bool -> Gen a 
nCoArb gen = flip coarbitrary gen 



-- 9)
colonPlusPlus :: (Eq a) => [a] -> [a] -> Bool
colonPlusPlus xs ys =
  (foldr (:) ys xs) == xs ++ ys

prop_colonPlusPlus :: Property
prop_colonPlusPlus =
  property $ (colonPlusPlus :: [Integer] -> [Integer] -> Bool)

-- This throws a type error because of type mismatch in the arguments of (==)
-- plusPlusConcat :: (Eq a) => [[a]] -> Bool
-- plusPlusConcat lxs = 
--   (foldr (:) [] lxs) == (concat lxs)
--    ^ expr :: [[a]]       ^ expr :: [a]
--  
--  The error here is that I used (:) instead of (++) for the foldr expr
--
--  TIP: Whenever an "infinite type" error occurs, check the type signatures
--       of all your expressions.


plusPlusConcat :: (Eq a) => [[a]] -> Bool
plusPlusConcat lxs =
  (foldr (++) [] lxs) == (concat lxs)

prop_plusPlusConcat :: Property
prop_plusPlusConcat = 
  property $ (plusPlusConcat :: [[Integer]] -> Bool)

-- Practice writing generators
nestedStringListGen :: Gen [[String]]
nestedStringListGen = listOf . listOf $ stringGen 

prop_plusPlusConcat' :: Property
prop_plusPlusConcat' = 
  forAll nestedStringListGen (\c -> plusPlusConcat c == True)

-- 10)

foo :: Int -> [a] -> Bool
foo n xs = 
  length (take n xs) == n

prop_foo :: Property
prop_foo =
  property $ (\num -> \ys -> foo (num :: Int) (ys :: String) == True )


-- 11)
readShow :: (Read a, Show a, Eq a) => a -> Bool
readShow x =
  (read . show $ x) == x

prop_readShow :: (Eq a, Read a, Show a) => Gen a -> Property
prop_readShow gen =
  forAll gen (\c -> readShow c == True)




--Tests

mainQuickCheck :: IO ()
mainQuickCheck = do
  putStrLn "halfIdentity" 
  quickCheck prop_halfIdentity

main :: IO ()
main = hspec $ do
  describe "halfIdentity" $ do
    it " 2 * (x / 2) = x" $ prop_halfIdentity
  describe "listOrdered" $ do
    it "Integers" $ prop_listOrdered_Integer
    it "Integers using QuickCheck Modifiers" $ prop_listOrdered_Integer'
    it "Float" $  prop_listOrdered_Float
    it "Char" $ prop_listOrdered_Char
  describe "plus" $ do
    it "associtativity" $  prop_plusAssociative
    it "commutativity" $ prop_plusCommutative
  describe "mult" $ do
    it "associativity" $ prop_multAssociative 
    it "commutativity" $ prop_multCommutative
  describe "quotRem" $ do
    it "(quot x y) * y + (rem x y) == x" $ prop_quotRemDecomp
  describe "divMod" $ do 
    it "(div x y) * y + (mod x y) == x" $ prop_divModDecomp
  describe "Exponentiation" $ do
    it "Non-associativity" $ prop_expoAssociative
    it "Non-commutativity" $ prop_expoCommutative
  describe "list reversal" $ do
    it "List of Integers" $ prop_listReversal integerListGen
    it "Strings" $ prop_listReversal stringGen
  describe "dollarSign: with generated functions" $ do
    it "String -> Integer" $ prop_dolladolla 
  describe "dot notation: with generated functions" $ do
    it "(Char -> Integer) -> (String -> Char)" $ prop_dotdot 
  describe "foldr" $ do
    it "foldr (:) vs, (++) with integer lists" $ prop_colonPlusPlus
    it "foldr (++) [] vs. concat with integer lists" $ prop_plusPlusConcat
    it "foldr (++) [] vs. concat with string lists" $ prop_plusPlusConcat'
  describe "function foo" $ do
    it "Strings" $ prop_foo
    it "List of Integers" $ property $
      (foo :: Int -> [Integer] -> Bool)
  describe "readShow" $ do
    it "Strings" $ prop_readShow stringGen
    it "Integers" $ prop_readShow integerGen
    it "List of Integers" $ prop_readShow integerListGen
