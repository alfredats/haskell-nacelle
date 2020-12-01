module Chapter17 where

import Control.Applicative
import Data.Char
import Data.Validation
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- defining applicative

-- class Functor f => Applicative f where
--    pure :: a -> f a
--    <*> :: f (a -> b) -> f a -> f b

--  * everything that can have an applicative instance must also have 
--    a functor instance.
--  * Applicative algebra implemented as instances in Haskell have 2 
--    methods: (1) pure, and (2) <*> a.k.a "apply"



-- Applicative Functors are Monoidal Functors

--  * The type signature of <*> introduces a "structure" aspect to the
--    function (a -> b), compared to normal <$>.
--      => An applicative functor has to combine possibly two different 
--         values of one type, and return a single value of the same type
--      => This can only occur if 'f' is a type with a Monoid instance
--         i.e. it has an implementation of mappend
--
--  Recall that monoids have a set of laws to be obeyed (Identity and 
--  Associativity). The Identity laws enable us to work with the "two 
--  values, same type" situation. 
--
--  This also enriches function application with the structure we are 
--  trying to map over. E.g.
--
--    [ (*2), (*3) ] <*> [4, 5] = [2*4, 2*5, 3*4, 3*5]
--                              = [8, 10, 12, 15]
--      
--      => Notice that the functions are enclosed within the same 
--         structure as the values that are to be applied to.
--      => The applicative functor allows for each function within the 
--         list to be mapped over to the values
--


-- Tuple Monoid and Applicative
-- 
-- With tuples and other binary (and higher) data structures, we can 
-- clearly see the monoidal nature of Applicative.
--
--    instance Monoid a => Applicative ((,) a) where
--        pure x = (mempty, x)
--        (u, f) <*> (v, x) = (mappend u f, f x)
--    
--  This yields:
--
--    ("Woo", (+1)) <*> (" Hoo!", 0) = ("Woo Hoo!", 1)
--
-- Note that the first elements of the tuples are monoidally joined, 
-- while the second elements result in a function application.



-- Applicatives in practice

-- List Applicatives 
--
--  Given f ~ [],
--    
--    pure :: a ->  f a 
--    pure :: a -> [] a 
--
--    (<*>) ::  f (a -> b) ->  f a ->  f b
--    (<*>) :: [] (a -> b) -> [] a -> [] b
--
--  With the list applicatives, we are able to map a plurality of functions
--  over a plurality of values. (<*>) takes each function value from the 
--  first list, applies the operations over the second list, before returning
--  a single output list. The fact that it doesn't return two seperate 
--  lists, or nested lists in which the original structures are preserved,
--  is due to the monoidal nature of the applicative.
--
--  Here's a more illustrative example (note the type signatures of groupings)
--    
--      (,) <$> [1, 2] <*> [3, 4] = f [1, 2] <*> [3, 4]
--        * with f = (,) <$>, f :: Functor j => j a -> j (b -> (a, b))
--      
--      f [1, 2] <*> [3, 4] = g <*> [3, 4]
--        * with g = f [1, 2] = [(1, ), (2, )],
--          g :: Num a => [b -> (a, b)]  
--        * Note the output of g is a list of functions
--                                
--      g <*> [3, 4] = [(1,3), (1,4), (2,3), (2,4)] 
--
--
--  Alternatively, we can write the same thing with the 'liftA2' function.
--  (Note: liftA2 requires the import of Control.Applicative)
--
--      liftA2 (,) [1,2] [3,4] = [(1,3), (1,4), (2,3), (2,4)] 
--
--  More examples:
--      
--      (+) <$> [1,2] <*> [3,5] = liftA2 (+) [1.2] [3,5]
--                              = [4, 5, 6, 7]
--
--      max <$> [1,2] <*> [1,4] = liftA2 (max) [1,2] [1,4]
--                              = [1, 4, 2, 4]
--
--  A working example:
--
--    (Think of lookup like key lookups in python dictionaries)

--    lookup :: Eq a => a -> [(a, b)] -> Maybe b 
lookupExample :: IO ()
lookupExample = 
  let l = lookup 3 [(3, "hello")]
      c (x:xs) = toUpper x: xs
  in do
    print l
    print $ length <$> l
    print $ c <$> l

--    Note: Haskell's Map data structures are equivalent to python's 
--          dictionaries. Import the Data.Map module to use them.

-- Exercise : Lookups 
--    refer to 17_lookups.hs


-- Exercise : Identity instance
newtype Identity a = Identity a 
  deriving (Eq, Ord, Show)

instance Functor Identity where 
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity g) (Identity x) = Identity $ g x

identityTest = hspec $ do
  describe "Identity" $ do
    it "const <$> Identity [1,2,3] <*> Identity [9,9,9]" $ do
      const <$> Identity [1,2,3] <*> Identity [9,9,9] `shouldBe` Identity [1,2,3]


-- Exercise : Constant Instance
newtype Constant a b =
  Constant { getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty 
  (<*>) (Constant x) (Constant y) = Constant $ x <> y


-- Maybe Applicative
--
-- When it comes to instances of Applicatives for Maybe, because the 
-- function to be applied is itself embedded within a Maybe structure, it
-- might not exist (i.e. f = Nothing). 
--
-- This will get quite messy, so a walkthrough has been implemented in
-- 17_maybeApplicative.hs
--
-- Exercise : Fixer Upper
--
-- 1) 
fixerUpper1 = const <$> Just "Hello" <*> pure "World"
-- 2)
fixerUpper2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]



-- Applicative laws

--  Law of Identity
--    pure id <*> v = v
applicativeIdentity :: IO ()
applicativeIdentity = do
  print $ pure id <*> [1..5]
  print $ pure id <*> Just "Hello Applicative"
  print $ pure id <*> (Nothing :: Maybe ())
  print $ pure id <*> (Left "Error'ish" :: Either [Char] ())
  print $ pure id <*> (Right 8001 :: Either () Int)
  print $ pure id <*> (+1) $ 2



--  Law of Composition
--    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

--  The law of composition for applicatives is quite similar to the one 
--  for Functors. They both state that the result of composing the 
--  functions then applying them, is the same as teh result of applying
--  the functions first then composing them. 
--
--  The 'pure' operator is used to ensure that the (.) composition 
--  operator is embedded in the appropriate structures, so that it can 
--  work with the <*> apply operator.

applicativeComposition :: IO ()
applicativeComposition = do
  print $ pure (.) <*> [(+1)] <*> [(+2)] <*> [1,2,3]
  print $ pure (.) <*> Just (+1) <*> Just (+2) <*> Just 1

--  The law of composition helps ensure there are no surprise results
--  from composing function applications


-- Law of Homomorphism
--    pure f <*> pure x = pure (f x)

--  A homomorphism is a structure-preserving map between two algebraic 
--  structures. The law essentially states that the effect of applying 
--  a function within some structure to a value embedded in the same 
--  structure, should ne the same as applying the function to the value
--  without affecting any outside structure.

applicativeHomomorphism :: IO ()
applicativeHomomorphism = do
  print $ pure (+1) <*> (pure 1 :: Maybe Int)
  print $ (pure ((+1) 1) :: Maybe Int)
  print $ (pure (+1) <*> pure 1 :: [Int])
  print $ (pure ((+1) 1) :: [Int])
  print $ (pure (+1) <*> pure 1 :: Either Char Int)
  print $ (pure ((+1) 1) :: Either Char Int)



-- Law of Interchange
--    u <*> pure y = pure ($ y) <*> u

--  The gist of this law is that it shouldn't matter whether the 
--  structure-embedded function is applied to the structure-embedded 
--  value from the left or from the right.

--  The right side of the definition deserves some explanation. By 
--  By sectioning the ($) function application operator together with 
--  the 'y', it creates and environment where the 'y' awaits a function
--  application. More concretely, if we explicitly list the types,

mPure :: a -> Maybe a
mPure = pure

embed :: Num a => Maybe ((a -> b) -> b)
embed = mPure ($ 2)

mApply :: Maybe ((a -> b) -> b) -> Maybe (a -> b) -> Maybe b
mApply = (<*>)

myResult = embed `mApply` Just (+2)

-- Examples
applicativeInterchange :: IO ()
applicativeInterchange = do
  print $ [(+1), (*2)] <*> pure 1
  print $ pure ($ 1) <*> [(+1), (*2)]
  print $ Just (+3) <*> pure 1
  print $ pure ($ 1) <*> Just (+3)




-- Property testing the applicative laws
--    refer to 17_checkers.hs 


-- zipList Monoid
--    concatenation is not the only way to monoidally combine lists.
--    we can zip the lists together, and monoidally combine the elements
--    in each index


-- Exercise : List Applicatives
--    refer to 17_listApplicative.hs

-- Exercise : ZipList Applicative
--    refer to 17_ziplistApplicative.hs


-- Applicatives for Either and Validation datatypes
--    Either and Validation are very similar, and only differ in how their
--    applicatives are defined. The Validation applicative monoidally 
--    combines errors (Left(s) in the case of Either), rather than 
--    forcing an exit prematurely.

--data Errors = 
    --DividedByZero
-- | StackOverflow
-- | MooglesChewedWires
  --deriving (Eq, Show)

--type VEI = Validation [Errors] Integer

--validationShowcase = do
  --print $ success == Success 2
  --print $ failure == Failure [StackOverflow]
  --print $ failure' == Failure [StackOverflow]
  --print $ failures == Failure [StackOverflow, MooglesChewedWires]
    --where success = Success (+1) <*> Success 1 :: VEI
          --failure = Success (+1) <*> Failure [StackOverflow] :: VEI
          --failure' = Failure [StackOverflow] <*> Success (+1) :: VEI
          --failures = Failure [StackOverflow] 
                  -- <*> Failure [MooglesChewedWires] :: VEI
          
-- Exercise : Variations on Either
--    refer to 17_validationApplicative.hs





-- Chapter Exercises

--  Given a type that has an instance of Applicative, specialize the
--  types of the methods.

--  1) []
--     
--     pure :: a -> [] a
--     (<*>) :: [] (a -> b) -> [] a -> [] b
--
--  2) IO
--
--     pure :: a -> IO a
--     (<*>) :: IO (a -> b) 
-- 
--  3) (,) a
--
--      pure :: Monoid a => b -> (,) a b
--      (<*>) :: Monoid x => (,) x (a -> b) -> (,) x a -> (,) x b
--
--  4)  (->) e
--
--      pure :: a -> (e -> a)
--      (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
--
--
--
--  Write instances for the following datatypes. Use the checkers 
--  library to validate the instances:
--
--  1) 
data Pair a = Pair a a deriving (Eq, Show)

instance Semigroup a => Semigroup (Pair a) where
  (<>) (Pair x y) (Pair a b) = Pair (x <> a) (y <> b)

instance Monoid a => Monoid (Pair a) where
  mempty = Pair mempty mempty
  mappend = (<>)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x 
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

type PairSI = (Pair String, Integer)
type PairSSI = Pair (String, String, Integer)

pairCheckers :: IO ()
pairCheckers = do
  quickBatch $ semigroup (trigger :: PairSI)
  quickBatch $ monoid (trigger :: Pair String)
  quickBatch $ functor (trigger :: PairSSI)
  quickBatch $ applicative (trigger :: PairSSI)
    where trigger = undefined


-- 2)
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two x y) = Two (a <> x) (b <> y)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance Monoid a => Functor (Two a) where
  fmap f (Two x y) = Two x $ f y

instance Monoid a => Applicative (Two a) where
  pure = Two mempty 
  (<*>) (Two a f) (Two x y) = Two (a <> x) $ f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq


type TwoSI = (Two String String, Integer)
type TwoSSI = Two String (String, String, Integer)

twoCheckers :: IO ()
twoCheckers = do
  quickBatch $ semigroup (trigger :: TwoSI)
  quickBatch $ monoid (trigger :: Two String String)
  quickBatch $ functor (trigger :: TwoSSI)
  quickBatch $ applicative (trigger :: TwoSSI)
    where trigger = undefined


-- 3)
data Three a b c = Three a b c deriving (Eq, Show)

instance (Monoid a, Monoid b) => Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three x y f) (Three a b c) = Three (x <> a) (y <> b) $ f c 

instance (Arbitrary a, Arbitrary b, Arbitrary c) => 
  Arbitrary (Three a b c) where
    arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

type ThreeSSI = Three String String (String, String, Integer)

threeCheckers :: IO ()
threeCheckers = do
  quickBatch $ functor (trigger :: ThreeSSI)
  quickBatch $ applicative (trigger :: ThreeSSI)
    where trigger = undefined


-- 4)
data Three' a b = Three' a b b deriving (Eq, Show)

instance Monoid a => Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a f g) (Three' x y z) = Three' (a <> x) (f y) (g z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

type SSI = (String, String, Integer)
type ThreeSSI' = Three' String SSI 

threeCheckers' :: IO ()
threeCheckers' = do
  quickBatch $ functor (trigger :: ThreeSSI')
  quickBatch $ applicative (trigger :: ThreeSSI')
    where trigger = undefined


-- 5 and 6 are very similar to 3 & 4 so imma skip
--
--
--
-- Combinations: Write a afunction to generate all possible combinations
-- on three input lists, using liftA3 from Control.Applicative
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,) 
