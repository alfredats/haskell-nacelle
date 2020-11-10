{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Chapter16 where

import Test.Hspec
import Test.QuickCheck

-- functors allow functions to be lifted over abstract structure.
-- 
-- class Functor f where
--    fmap :: (a -> b) -> f a -> f b

-- Exercises : Be kind
-- 
-- 1) Kind of 'a' is *
-- 2) kind of 'b' is * -> *. The expression (b a) has kind *, 
--    so T (b a) implies that 'T' must have kind * -> *.
-- 3) Kind of 'c' is * -> * -> *.


data FixMePls a =
    FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- fmap (+1) (Pls 1) == Pls 2

-- Because data constructors (like Just, Left etc.) are functions 
-- as well, we can apply them using fmap

-- Interestingly,
myLeft = Left "hello" :: Either String Int
myRight = Right 111 :: Either String Int

fmapLeft = fmap (Just) myLeft -- returns Left "hello" :: Either String (Maybe Int)
fmapRight = fmap (Just) myRight -- returns Right (Just 111) :: Either String (Maybe Int
-- But
-- fmapNothing = fmap (Nothing) myLeft -- doesn't compile due to Nothing :: Maybe a
--
-- Note to self : refer to functor definition for Maybe and Either


-- Functor Laws

-- 1) Identity [fmap id == id]
-- 2) Composition [fmap (f . g) == fmap f . fmap g]
-- 3) Structure preservation [refer to chapter 16.5 of haskellbook]

-- Given
n = Nothing
w = Just "woohoo"
ave = Just "ave"
lms = [ave, n, w]

replaceWithP = const 'p' -- const :: a -> b -> a
-- (fmap . fmap) replaceWithP lms = [Just 'p', Nothing, Just 'p']
--
-- What is the type of (fmap . fmap)?
--
-- Writing the type signatures explicity,
-- (.) :: (b -> c) -> (a -> b) -> a -> c
fmap' :: (v -> w) -> [] v -> [] w ; fmap' = fmap
fmap'' :: (x -> y) -> Maybe x -> Maybe y ; fmap'' = fmap

-- (.) fmap' :: (a -> v -> w) -> a -> [v] -> [w]
-- (fmap' . fmap'') :: (x -> y) -> [] (Maybe x) -> [] (Maybe y)
-- therefore,
--  (fmap  . fmap) has the type
--    Functor f, Functor g => (x -> y) -> f (g x) -> f (g y)


-- Exercise : Heavy Lifting
-- 1)
a = fmap (+1) $ read "[1]" :: [Int]
-- 2)
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"]) 
-- 3)
c = ((*2) . (\x -> x - 2))
-- 4) 
d =
  ((return '1' ++) . show) . (\x -> [x, 1..3])
-- 5)
-- e has type IO Integer.
--  * '(*3)' has type of 'Integer -> Integer' so we need an fmap
--  * 'e = fmap (*3) changed' implies 'changed' should have type
--    IO Integer as well
--  * In 'changed = read ("123"++) show ioi', read has type 'String -> a'
--      this implies that we need `fmap read X` where X :: IO String
--  * `X = ("123"++) show ioi :: IO String` implies presence of fmap
--    since ("123"++) :: String -> String 
--      therefore X should be `fmap ("123"++) Y` where Y :: IO String
--  * `Y = show ioi :: IO String implies presence of fmap because 
--    show :: a -> String, and ioi :: IO Integer
--      therefore X should be `fmap show ioi`

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read (fmap ("123"++) (fmap show ioi))
    in fmap (*3) changed

heavyLifting :: IO ()
heavyLifting = hspec $ do
  describe "Exercise: Heavy Lifting" $ do
    it "a == 2" $ do
      a `shouldBe` [2]
    it "b == Just [\"Hi,lol\", \"Hellolol\"]" $ do
      b `shouldBe` (Just ["Hi,lol","Hellolol"])
    it "c 1 == -2" $ do
      c 1 `shouldBe` -2
    it "d 0 == \"1[0,1,2,3]\"" $ do
      d 0 `shouldBe` "1[0,1,2,3]"
    it "e == 3693" $ do
      x <- e
      x `shouldBe` 3693


-- Functor Quickcheck properties

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
  (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = 
  (fmap g (fmap f x)) == (fmap (g . f) x)

prop_functorIdentity :: IO () 
prop_functorIdentity = quickCheck $ (\s -> functorIdentity (s :: [Int]))

prop_functorCompose :: IO ()
prop_functorCompose = quickCheck $ 
  (\s -> functorCompose (+1) (*2) (s :: [Int]))

functorLaws :: IO ()
functorLaws = do 
  prop_functorIdentity
  prop_functorCompose




-- Exercise : Instances of Func
-- 1)
newtype Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity x) (Identity y) =
    x == y

instance Show a => Show (Identity a) where
  show (Identity x) = "Identity " ++ show x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap (Identity) $ arbitrary

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)


-- 2)
data Pair a = Pair a a 

instance Eq a => Eq (Pair a) where
  (==) (Pair v w) (Pair x y) =
    (v == x) && (w == y)

instance Show a => Show (Pair a) where
  show (Pair x y) = "Pair " ++ show x ++ show y

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do 
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)


-- 3)
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)


-- 4)
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do 
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return (Three x y z)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)


-- 5)
data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Three' a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return (Three' x y z)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)


-- 6 & 7 are conceptually similar to 4 & 5

-- 8) An instance of functor for Trivial cannot be implemented because
--    there is no structure to lift.


funcInstances :: IO ()
funcInstances = hspec $ do
  describe "Identity a" $ do
    it "Functor Identity" $ do
      property $ (\s -> functorIdentity (s :: Identity Int))
    it "Functor composition" $ do
      property $ (\s -> functorCompose (+1) (*2) (s :: Identity Int))
  describe "Pair a" $ do
    it "Functor Identity" $ do
      property $ (\s -> functorIdentity (s :: Pair Int))
    it "Functor composition" $ do
      property $ (\s -> functorCompose (+1) (*2) (s :: Pair Int))
  describe "Two a b" $ do
    it "Functor Identity" $ do
      property $ (\s -> functorIdentity (s :: Two String Int))
    it "Functor composition" $ do
      property $ (\s -> functorCompose (+1) (*2) (s :: Two String Int))
  describe "Three a b c" $ do
    it "Functor Identity" $ do
      property $ (\s -> functorIdentity (s :: Three String Char Int))
    it "Functor composition" $ do
      property $ (\s -> 
        functorCompose (+1) (*2) (s :: Three String Char Int))
  describe "Three' a b b" $ do
    it "Functor Identity" $ do
      property $ (\s -> functorIdentity (s :: Three String Int Int))
    it "Functor composition" $ do
      property $ (\s -> 
        functorCompose (+1) (*2) (s :: Three String Int Int))



-- Ignoring Possibilities

-- Maybe a
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)

showMaybe'' :: Show a => Maybe a -> Maybe String
showMaybe'' = fmap show

  -- general case
liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

-- Exercise : Possibly
data Possibly a = LolNope
                | Yeppers a
                deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)


-- Short Exercise

-- 1) Write a Functor instance for a datatype identical to Either.
data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where 
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

-- 2) Why is a functor instance that applies a function only to First
--    (Either's left) impossible?
--
--    Because the functor acts on (Sum a), where a is the type 
--    encompassed by First. 



-- A Peculiar Functor
newtype Constant a b = 
  MyConst { getConstant :: a}
  deriving (Eq, Show)

instance Functor (Constant m) where -- (Constant m) is the structure being lifted
  fmap _ (MyConst v) = MyConst v -- but MyConst is the data constructor that's acted upon

testConstant :: IO ()
testConstant = do
  putStrLn "" 
  putStrLn $ "const 2 (getConstant (MyConst 3)) = " ++ show (const 2 (getConstant (MyConst 3)))
  putStrLn $ "fmap (const 2) (MyConst 3) = " ++ show (fmap (const 2) (MyConst 3))
  putStrLn ""

-- because of the phantom type variable b in the Const type constructor,
-- there are no values of the type that the instance of Functor for (Constant m)
-- is supposed to be mapping. This ends up with the 'const 2' expression 
-- not being applied at all.

-- Making sure 'Constant a b' adheres to functor laws
constIdentity :: Property
constIdentity = property $ (\s -> 
  (id (MyConst (s :: Int))) == (fmap id (MyConst s)))

constComposition :: Property
constComposition = property $ (\s ->
  (fused (MyConst (s :: Int))) == (seperate (MyConst s)))
    where c = const 3
          c' = const 5
          fused = fmap (c . c')
          seperate = (fmap c) . (fmap c')


-- nested(?) structures
data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor d => Functor (Wrap d) where
  fmap f (Wrap fa) = Wrap (fmap f fa) -- fa = d in instance specification


-- Transforming Structure

type Nat f g = forall a . f a -> g a -- this requires the language extension RankNTypes

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- This will not work. If you want to transform the values, write a fold instead
degenerateMtL :: Nat Maybe []
degenerateMtL Nothing = []
-- degenerateMtL (Just a) = [a + 1]





-- Chapter Exercises

-- Determine if the following datatypes can have a valid Functor instance
-- 
-- 1) Data Bool = False | True
--
--    Nope. Bool is a nullary data type, but functors can only be written
--    for unaries & above.
--
--  2) data BoolAndSomethingElse a = False' a | True' a
--
--    Yeap. This is a unary data type
--
--  3) data BoolAndMaybeSomethingElse a = Falsish | Truish a
--
--    Yeap. This is also a unary data type. See above for an example.
--
--  4) newtype Mu f = InF {outF :: f (Mu f)}
--
--    Hmm. This one has a unary type constructor, and is somewhat similar
--    to the Wrap datatype above. However, lifting the functor in this 
--    case might result in an infinite traversal of the Mu data structure.
--    In this case I am inclined to say that it isn't possible.
--
--    Cross-ref: (Maryam) -> Links a stackoverflow page that says no,
--                           but for a different reason. The reason being
--                           that Mu is of type (* -> *) -> *, and functors
--                           need type * -> *.
--
--  5) data D = D (Array Word Word) Int Int
--
--    Type constructor D is nullary. No valid functor instance.
--
--
-- Rearranve the arguments to the type constructor of the datatype so 
-- the Functor instance works
--
-- 1)
data Sum' b a = First' a | Second' b deriving (Eq, Show)

instance Functor (Sum' e) where
  fmap f (First' a) = First' $ f a
  fmap _ (Second' b) = Second' b

-- 2) 
data Company a c b = DeepBlue a c | Something b deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3)
data More b a = L a b a 
              | R b a b 
              deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'



-- Write Functor instances for the following datatypes

-- 1)
data Quant a b = Finance
                | Desk a
                | Bloor b
                deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = frequency [ (1, return Finance)
                        , (1, fmap (Desk) $ arbitrary)
                        , (1, fmap (Bloor) $ arbitrary) ]

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor y) = Bloor $ f y

-- 2) This is just the Constant datatype illustrated previously
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

-- 3)
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

-- We define a "Flip" data type that allows us to create different 
-- Functor behavior i.e. modifying the value of the "inner" datatype
instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip $ K (f x) 

instance Arbitrary a => Arbitrary (Flip K b a) where
  arbitrary = do 
    x <- arbitrary
    return (Flip $ K x)

-- 4)
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = fmap (GoatyConst) $ arbitrary

-- 5)
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = LiftItOut $ g <$> x 

instance Arbitrary a => Arbitrary (LiftItOut Identity a) where
  arbitrary = do
    a <- arbitrary
    return (LiftItOut (Identity a))

-- 6)
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa x y) = DaWrappa (fmap h x) (fmap h y)

instance Arbitrary a => Arbitrary (Parappa Maybe Identity a) where
  arbitrary = do 
    a <- arbitrary 
    return (DaWrappa (Just a) (Identity a))

-- 7)
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething x y) = 
    IgnoringSomething x (fmap h y)

-- 8)
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap h (Notorious x y z) = Notorious x y $ fmap h z

-- 9)
data List a = Nil
            | Cons a (List a) 
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x (y)) = Cons (f x) $ fmap f y

-- 10)
data GoatLord a = 
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) 
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = 
    MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11)
data TalkToMe a = 
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x a) = Print x $ f a
  fmap f (Read g) = Read $ fmap f g

instanceLaws :: IO ()
instanceLaws = hspec $ do
  describe "Sum a b" $ do
    it "Functor Identity" $ do
      property $ (\x -> functorIdentity (x :: Quant Int Int))
    it "Functor Composition" $ do
      property $ (\x -> functorCompose (+1) (*2) (x :: Quant Int Int))
  describe "K a b" $ do
    it "Functor Identity" $ do
      property $ (\s -> 
        (id (K (s :: Int))) == (fmap id (K s)))
    it "Functor Composition" $ do
      property $ (\s ->
        (fmap (const 3 . const 5) (K (s :: Int))) == 
          ((fmap (const 3) . fmap (const 5)) (K s)))
  describe "Flip K a" $ do
    it "Functor Identity" $ do
      property $ (\x -> functorIdentity (x :: Flip K String Int)) 
    it "Functor Composition" $ do
      property $ (\x -> functorCompose (+1) (*2) (x :: Flip K String Int))
  describe "EvilGoateeConst a" $ do
    it "Functor Identity" $ do
      property $ (\x -> functorIdentity (x :: EvilGoateeConst String Int)) 
    it "Functor Composition" $ do
      property $ (\x -> functorCompose (+1) (*2) (x ::EvilGoateeConst String Int))
  describe "LiftItOut Identity a" $ do
    it "Functor Identity" $ do
      property $ (\x -> functorIdentity (x :: LiftItOut Identity Int))
    it "Functor Composition" $ do
      property $ (\x -> functorCompose (+1) (*2) (x :: LiftItOut Identity Int))
  describe " Parappa Maybe Identity a" $ do
    it "Functor Identity" $ do
      property $ (\x -> functorIdentity (x :: Parappa Maybe Identity Int))
    it "Functor Composition" $ do
      property $ (\x -> functorCompose (+1) (*2) (x :: Parappa Maybe Identity Int))
-- i got lazy
