module Chapter15 where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid
import Control.Monad

-- A monoid is, simply, a function (or operation) that takes two arguments 
-- and follows the laws of identity and associativity. Identity meaning 
-- that there exists some value that when utilized with a second value in
-- an operation, returns the second value (i.e. identity of the second
-- value)

-- It should be noted that a monoid are formed with respect to an 


-- Exercise : Optional Monoid
data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) x Nada = x
  (<>) Nada x = x 
  (<>) (Only a) (Only b) = Only $ a <> b 

instance Monoid a => Monoid (Optional a) where
  mempty = Nada 
  mappend = (<>)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [ (1, return Nada)
                        , (1, fmap (Only) $ arbitrary)]

optionalMonoid :: IO ()
optionalMonoid = hspec $ do
  describe "Optional Monoid" $ do
    it "Only (Sum a)" $ do
      mappend onlySum onlySum `shouldBe` Only (Sum {getSum = 2})
    it "Only (Product a)" $ do
      mappend onlyFour onlyTwo `shouldBe` Only (Product {getProduct = 8})
    it "Only (Sum a) and Nada" $ do
      mappend onlySum Nada `shouldBe` onlySum 
    it "Only [Integer] and Nada" $ do
      mappend (Only [1]) Nada `shouldBe` Only [1]
    it "Nada and Only (Sum a)" $ do
      mappend Nada onlySum `shouldBe` onlySum
    where 
      onlySum = Only (Sum 1)
      onlyTwo = Only (Product 2)
      onlyFour = Only (Product 4)


-- Exercise : Madlib Madness
--    eh. just stuff everything into a list and pass it through mconcat

-- Exercise : Maybe another Monoid 

newtype First' a = 
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup a => Semigroup (First' a) where
  (<>) (First' {getFirst' = x}) (First' {getFirst' = y}) =
    (First' {getFirst' = (x <> y)})

instance Semigroup a => Monoid (First' a) where
  mempty = First' {getFirst' = Nada}

--instance Arbitrary a => Arbitrary (First' a) where
  --arbitrary = do
    --x <- onlyGen
    --return (First' x)
    --where 
      --onlyGen :: Arbitrary a => Gen (Optional a)
      --onlyGen = do
        --x <- arbitrary
        --frequency [ (1, return Nada)
                  --, (1, return (Only x)) ] 

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = frequency [ (1, return $ First' Nada)
                        , (1, (First' . Only) `fmap` arbitrary)]


type MA = String -> String -> String -> Bool
type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = 
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

maybeAnother :: IO ()
maybeAnother = hspec $ do
  describe "Maybe another Monoid" $ do
    it "Associativity" $ property $ (monoidAssoc :: FirstMappend)
    it "Left Identity" $ property $ (monoidLeftIdentity :: FstId)
    it "Right Identity" $ property $ (monoidRightIdentity :: FstId)



-- Chapter exercises

-- Semigroup & Monoid Exercises

-- 1)
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = 
  (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivIden = Trivial -> Bool

-- 2)
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity $ x <> y

instance Monoid a => Monoid (Identity a) where
  mempty = Identity (mempty)
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap (Identity) $ arbitrary

type IdenAssoc = Identity String -> Identity String -> 
                 Identity String -> Bool
type IdenIden = Identity String -> Bool

-- 3)

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two v w) (Two x y) = Two (v <> x) (w <> y)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two (mempty) (mempty)
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoSSI = Two String (Sum Int)

type TwoAssoc = TwoSSI -> TwoSSI -> TwoSSI -> Bool
type TwoIden = TwoSSI -> Bool

-- 4)
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
  Semigroup (Three a b c) where
    (<>) (Three s t u) (Three x y z) = Three (s <> x) (t <> y) (u <> z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return (Three a b c)

type ThreeTT = Three (Sum Int) (Product Int) (String)
type ThreeAssoc = ThreeTT -> ThreeTT -> ThreeTT -> Bool


-- 5)
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d) where
    (<>) (Four m n o p) (Four w x y z) = 
      Four (m <> w) (n <> x) (o <> y) (p <> z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary 
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return (Four a b c d)

type FourTT = Four (Ordering) (Sum Int) (Product Int) (String) 
type FourAssoc = FourTT -> FourTT -> FourTT -> Bool


-- 6)
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) _ (BoolConj False) = BoolConj False
  (<>) (BoolConj False) _ = BoolConj False
  (<>) _ _ = BoolConj True

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = fmap (BoolConj) $ arbitrary

type BoolConjAssoc = BoolConj-> BoolConj-> BoolConj-> Bool
type BoolConjIden = BoolConj -> Bool



-- 7)
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj True) _ = BoolDisj True 
  (<>) _ (BoolDisj True) = BoolDisj True 
  (<>) _ _ = BoolDisj False 

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend x y = x <> y

instance Arbitrary BoolDisj where
  arbitrary = fmap (BoolDisj) $ arbitrary

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolDisjIden = BoolDisj -> Bool


-- 8)
data Or a b = Fst a 
            | Snd b
            deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (<>) (Snd x) _ = Snd x
  (<>) _ (Snd y) = Snd y
  (<>) (Fst x) (Fst y) = Fst y

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Or a b) where
    arbitrary = frequency [ (1, fmap (Fst) $ arbitrary)
                          , (1, fmap (Snd) $ arbitrary) ]

type OrTT = Or (Sum Int) (Product Int)

type OrAssoc = OrTT -> OrTT -> OrTT -> Bool



-- 9)
-- (f <> g) x == (f x) <> (g x)
newtype Combine a b =
  Combine {unCombine :: (a -> b)} 

instance Show (Combine a b) where 
  show (Combine _) = "Combine"

--instance Eq b => Eq (Combine a b) where
  --(==) (Combine f) (Combine g) = f == g

instance Semigroup b => Semigroup (Combine a b) where -- we only need the semigroup b type constraint
  (<>) (Combine f) (Combine g) = Combine (f <> g)     -- as at the end of the day, "b" is what we end
                                                      -- up with & is the only type that matters

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty 
  mappend = (<>)


instance (CoArbitrary a, Arbitrary b) =>
  Arbitrary (Combine a b) where
    arbitrary = fmap (Combine) $ arbitrary

f :: Combine Integer (Sum Integer)
f = Combine $ \n -> Sum (n + 1)
g :: Combine Integer (Sum Integer)
g = Combine $ \n -> Sum (n-1)

combineSpecTest :: IO ()
combineSpecTest = hspec $ do
  describe "Prelim tests for Combine" $ do
    it "f <> f $ 1 = 4" $ do
      (getSum $ unCombine (f <> f) 1) `shouldBe` 4
    it "g <> g $ 1 = 0" $ do
      (unCombine (g <> g) 1) `shouldBe` Sum {getSum = 0}
    it "g <> f $ 1 = 2" $ do
      (unCombine (g <> f) 1) `shouldBe` Sum {getSum = 2}
    it "f <> g $ 1 = 2" $ do
      (unCombine (f <> g) 1) `shouldBe` Sum {getSum = 2}

type CombineTT a = Combine a String

type CombineAssoc = Integer -> CombineTT Integer -> CombineTT Integer -> 
                    CombineTT Integer -> Bool
type CombineIden = Integer -> CombineTT Integer -> Bool

combineAssoc :: a -> CombineTT a -> CombineTT a -> CombineTT a -> Bool
combineAssoc v a b c = 
  (unCombine (a <> (b <> c)) v) == (unCombine ((a <> b) <> c) v)

combineRightIden :: a -> CombineTT a -> Bool
combineRightIden v a =
  (unCombine (mempty <> a) v) == unCombine a v

combineLeftIden :: a -> CombineTT a -> Bool
combineLeftIden v a =
  (unCombine (a <> mempty) v) == unCombine a v


-- 10)
newtype Comp a =
  Comp {unComp :: (a -> a)}

instance Show (Comp a) where
  show (Comp _) = "Comp"

instance Semigroup a => Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp (f <> g)

instance Monoid a => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

instance (CoArbitrary a, Arbitrary a) =>
  Arbitrary (Comp a) where
    arbitrary = fmap (Comp) $ arbitrary

type CompAssoc = String -> Comp String -> Comp String -> 
                 Comp String -> Bool
type CompIden = String -> Comp String -> Bool


compAssoc :: (Semigroup a, Eq a) => a -> Comp a -> Comp a -> Comp a -> Bool
compAssoc x f g h =
  (unComp (f <> (g <> h)) x) == (unComp ((f <> g) <> h) x)

compRightIden :: (Monoid a, Eq a) => a -> Comp a -> Bool
compRightIden v a =
  (unComp (mempty <> a) v) == unComp a v

compLeftIden :: (Monoid a, Eq a) => a -> Comp a -> Bool
compLeftIden v a =
  (unComp (a <> mempty) v) == unComp a v



-- 11)
data Validation a b = Foil a
                    | Socc b
                    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Socc x) _ = Socc x
  (<>) _ (Socc y) = Socc y
  (<>) (Foil x) (Foil y) = Foil (x <> y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = frequency [ (1, fmap (Foil) $ arbitrary)
                        , (1, fmap (Socc) $ arbitrary) ]

type ValidTT = Validation String Int

type ValidAssoc = ValidTT -> ValidTT -> ValidTT -> Bool





semigroupExercises :: IO ()
semigroupExercises = hspec $ do
  describe "Semigroups: Trivial" $ do
    it "Association" $ property $ (semigroupAssoc :: TrivAssoc)
    it "Left Identity" $ property $ (monoidLeftIdentity :: TrivIden)
    it "Right Identity" $ property $ (monoidRightIdentity :: TrivIden)
  describe "Semigroups : Identity a" $ do
    it "Association" $ property $ (semigroupAssoc :: IdenAssoc)
    it "Left Identity" $ property $ (monoidLeftIdentity :: IdenIden)
    it "Right Identity" $ property $ (monoidRightIdentity :: IdenIden)
  describe "Semigroups : Two a b" $ do
    it "Association" $ property $ (semigroupAssoc :: TwoAssoc)
    it "Left Identity" $ property $ (monoidLeftIdentity :: TwoIden)
    it "Right Identity" $ property $ (monoidRightIdentity :: TwoIden)
  describe "Semigroups : Three a b c" $ do
    it "Association" $ property $ (semigroupAssoc :: ThreeAssoc)
  describe "Semigroups : Four a b c d" $ do
    it "Association" $ property $ (semigroupAssoc :: FourAssoc)
  describe "Semigroups : BoolConj" $ do
    it "Association" $ property $ (semigroupAssoc :: BoolConjAssoc)
    it "Left Identity" $ property $ (monoidLeftIdentity :: BoolConjIden)
    it "Right Identity" $ property $ (monoidRightIdentity :: BoolConjIden)
  describe "Semigroups : BooDisj" $ do
    it "Association" $ property $ (semigroupAssoc :: BoolDisjAssoc)
    it "Left Identity" $ property $ (monoidLeftIdentity :: BoolDisjIden)
    it "Right Identity" $ property $ (monoidRightIdentity :: BoolDisjIden)
  describe "Semigroups : Or a b" $ do
    it "Association" $ property $ (semigroupAssoc :: OrAssoc)
  describe "Semigroups : Combine a b (functions)" $ do
    it "Association" $ property $ (combineAssoc :: CombineAssoc)
    it "Left Identity" $ property $ (combineLeftIden :: CombineIden)
    it "Right Identity" $ property $ (combineRightIden :: CombineIden)
  describe "Semigroups : Comp a (functions)" $ do
    it "Association" $ property $ (compAssoc :: CompAssoc)
    it "Left Identity" $ property $ (compLeftIden :: CompIden)
    it "Right Identity" $ property $ (compRightIden :: CompIden)
  describe "Semigroups : Validation a b" $ do
    it "Association" $ property $ (semigroupAssoc :: ValidAssoc)

linesep :: IO () 
linesep = putStrLn "--------------------------------------------------"

main :: IO () 
main = do
  linesep
  optionalMonoid
  linesep
  maybeAnother
  linesep
  semigroupExercises 
  linesep
