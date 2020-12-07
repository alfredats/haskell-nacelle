{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Chapter18 where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Monads!
--
--  We're finally here, at the topic that everyone writes articles about.
--
--  Typedef of Monad:
--    
--    class Applicative m => Monad m where 
--      (>>=)  :: m a -> (a -> m b) -> m b
--      (>>)   :: m a -> m b -> m b
--      return :: a -> m a

--  Monads are stronger than Applicatives, which is in turn stronger than
--  Functors. This allows us to derive Applicatives and Functors in terms
--  of Monad. Concretely, this means we can write fmap in terms of monadic
--  operations. 
--
--      'fmap f xs' is equivalent to 'xs >>= return . f'
--  
--  Looking at the type signatures of (>>=) and 'return', it's easy to 
--  see. 
--        
--      return :: Monad m => a -> m a
--      f :: a -> b
--      (return . f) :: Monad m => a -> m b
--
--  The type signature of (return . f) is exactly what (>>=) takes. And 
--  with (return . f) settled, the behavior of (>>=) is exactly the same
--  as (fmap f). 
--
--
--
--  CORE OPERATIONS OF MONADS
--
--  The minimally complete Monad instance only requires the (>>=) 
--  operation defined. 'return' is essentially 'pure' from Applicative,
--  while (>>) a.k.a. the sequencing operator, sequences two actions while
--  discarding the result of the first. Lastly, (>>=) is known as the 
--  bind operator.
--
--  The bind operator has a type signature that is similar to fmap and 
--  (<*>). How it differs, is that monadic bind takes a function that 
--  introduces structure i.e. (a -> f b), applies it to the input values
--  that have the same monadic structure (f a), and returns values that
--  have one layer of structure discarded (f b instead of f (f b)).
--
--  This is largely due to the existence of the monadic join operation,
--  which is the monadic implementation of concat.
--
--      join :: Monad m => m (m a) -> m a
--
--
--  In summary, monadic bind takes a function that alters the structure
--  of the original input, then flattens the two layers of structure into
--  one.


-- Exercise : The answer is the exercise. Write bind in terms of fmap
--            and join.

bind :: Monad m => (a -> m b) -> m a -> m b
bind f xs = join $ fmap f xs    


-- Monads also lift!
--
--  The Monad class also includes a set of 'lift' functions. They work 
--  exactly like the ones in Applicative, but exist due to being
--  grandfathered in before Applicatives were a thing.
--
--  The lifting functions are 'liftM', 'liftM2', 'liftM3'.




-- Do Syntax & Monads
--
--  It's important to remember that 'do' syntax works with any monad, 
--  not just IO. However, strictly speaking it is syntactic sugar, and
--  not necessarily required to sequence actions.
--
--  Like the lift operators, there is an equivalent for the sequencing 
--  operator of Monad within Applicative.
--    
--    (*>) :: Applicative f => f a -> f b -> f b
--    (>>) :: Monad m       => m a -> m b -> m b
--
--  We can see what 'do' syntax does by manually transforming it.
strseq :: IO ()
strseq = do 
  putStrLn "blah" 
  putStrLn "another thing"

strseq' :: IO ()
strseq' = 
  putStrLn "blah" >>
  putStrLn "another thing"

strseq'' :: IO ()
strseq'' = 
  putStrLn "blah" *>
  putStrLn "another thing" 

--  The same can be done with the variable binding that do syntax uses.

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = 
  getLine >>= putStrLn



-- When fmap alone is not enough
--
--  Consider the following expression:
--    
--    putStrLn <$> getLine
--
--  Running the expression in ghci allows you input, but it does not 
--  actually print whatever you're typed in. To understand why, let's 
--  look at the type signatures.
--
--    getLine  :: IO String
--    putStrLn :: String -> IO ()
--    (<$>)    :: Functor f => (a -> b) -> f a -> f b
--
--  putStrLn is the first argument to fmap, so we expect the type signature
--  of the (<$>) operator within the initial expression to be as follows:
--
--    (String -> IO ()) -> IO String -> IO (IO ())
--
--  The outer IO of the output type belongs to the getLine, while the
--  inner IO (wrapped by parentheses) belongs to the putStrLn expression.
--
--  TODO :: WHY DOESN'T IO (IO ()) work? 
--    - The main reason I can think of is that you can't wrap IO in a 
--      structure that already has IO? I really don't understand how IO
--      works...
--
--  To fix this, we merge the effects of getLine and putStrLn into a 
--  single IO action with the monadic 'join' function.
--
--    join $ putStrLn <$> getLine
--
--  The resultant merged IO action consists of the effects of the two IO
--  actions, nested in order. 


-- An example of how do syntax simplifies things
twoBinds :: IO () 
twoBinds = 
  putStrLn "name pls:" >>
    getLine >>= 
      \name ->
      putStrLn "age pls:" >>
          getLine >>=
            \age -> putStrLn $ "y helo thar, " ++ 
              name ++ " who is " ++ age ++ "years old"

doTwoBinds :: IO ()
doTwoBinds = do
  putStrLn "name pls: "
  name <- getLine

  putStrLn "age pls: "
  age <- getLine

  putStrLn $ "y helo thar, " ++ 
    name ++ " who is " ++ age ++ "years old"

-- askName :: IO ()
-- askAge  :: IO ()
-- printNameAge :: IO ()
-- (>>)  :: IO () -> IO ? -> IO ?
-- (>>=) :: IO String -> (a -> IO ()) -> IO ()
twoBinds' :: IO ()
twoBinds' = 
  askName >>
    getLine >>= 
      \name -> askAge >> 
        getLine >>= 
          \age -> putStrLn ("helo there, " ++
            name ++ ", aged " ++ age)
    where askName :: IO ()
          askName = putStrLn "name pls? "
          askAge :: IO ()
          askAge = putStrLn "age pls? "


-- Examples of Monad use
--
--  List
--  
--  (>>=)  :: [] a -> (a -> [] b) -> [] b
--  return :: a -> [] a
--
twiceWhenEven :: [] Integer -> [] Integer
twiceWhenEven xs = do
  x <- xs -- note the interaction between (<-) and xs here
  if even x
     then [x*x, x*x]
     else [x*x]

--  The (x <- xs) line binds individual values out of the list input.
--  The if-then-else expression is the (a -> m b), taking individual 
--  values out and possibly generating more values, resulting in a size
--  increase of the list. 

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = 
  xs >>= 
    \x -> if even x then [x*x, x*x] else [x*x]

--  This is equivalent to twiceWhenEven. xs :: [Integer], (>>=) extracts
--  individual a :: Integer, passes it through the lambda function, and
--  the outputs are joined back together.
--
--
--
--  Maybe
--
--  (>>=)  :: Maybe a -> (a -> Maybe a) -> Maybe b
--  return :: a -> Maybe a
--
--  The monad instance of Maybe looks very similar to its Applicative 
--  instance, but there are some differences. 

data Cow = Cow {
    name    :: String,
    age     :: Int,
    weight  :: Int
               } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int 
noNegative n = if n >= 0 then Just n else Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c = 
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = 
  case noEmpty name' of 
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy   <- noEmpty name'
  agey    <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

-- Both mkSphericalCow and mkSphericalCow' should give the same results
-- We can also write it with (>>=)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' = 
  noEmpty name' >>=
    \nammy -> 
      noNegative age' >>=
        \agey ->
          noNegative weight' >>=
            \weighty ->
              weightCheck (Cow nammy agey weighty)

-- We can't, however, do it with Applicative. The "layer" of monadic 
-- structure present within the mkSphericalCow function, and how the 
-- Cow argument to weightCheck could possibly contain 'Nothing's, lends 
-- itself only to the (>>=) operator and the corresponding (a -> m b) 
-- function input. 

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer 
g i = if even i then Just (i + 1) else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do 
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

doSomething'' n = 
  f n >>= 
    \a -> g a >>=
      \b -> h b >>=
        \c -> pure (a, b, c)

doSomethingApplicative n = do -- This doesn't give what you need
  a <- f n
  b <- Just g <*> f n
  c <- Just h <*> b
  pure (a, b, c)

--  Very simply, with the Maybe Applicative, each Maybe computation fails
--  or succeeds independently of one another. The result of one function
--  does not affect the final result. However with the Maybe Monad, the 
--  results of prior computations affect the intermediary computations and
--  thus the final result. 

--    Passing a name argument of "" to mkSphericalCow'' yields the 
--    following computational steps:
--
--     1. mkSphericalCow'' "" 5 499 = 
--          noEmpty "" >>==
--            \nammy -> ....
--
--     2. mkSphericalCow'' "" 5 499 =
--          Nothing >>=                 (since noEmpty "" = Nothing)
--            \nammy -> ...
--
--     3. mkSphericalCow'' "" 5 499 =
--          Nothing                     (since Nothing >>= _ = Nothing,
--                                       referring to the Monad instance
--                                       of Maybe)
--
--  The rest of the computations are discarded, and it saves us having 
--  to case-match over the Nothing on all the functions, only to return
--  a Nothing value.
--
--
--
--  Either
--
--  (>>=)  :: Either e a -> (a -> Either e b) -> Either e b
--  return :: a -> Either e a
--
--  Here's an implementation of the Either monad.

type Founded = Int
type Coders  = Int

data SoftwareShop = 
  Shop {
      founded     :: Founded
    , programmers :: Coders 
       } deriving (Eq, Show)

data FoundedError = 
    NegativeYears Founded 
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n 
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n 
  | n < 0     = Left $ NegativeCoders n
  | n > 5000  = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware years coders = do
  founded     <- validateFounded years
  programmers <- validateCoders coders 
  if programmers > div founded 10
     then Left $ TooManyCodersForYears founded programmers
     else Right $ Shop founded programmers

-- mkSoftware short-circuits on the first computation to have failed. 
-- This is due to the nature of monadic computations where later values 
-- can depend on previous ones. 
--
--  mkSoftware (-1) 0 yields Left (NegativeYears (-1))
--  mkSoftware 0 (-1) yields Left (NegativeCoders (-1))
--  mkSoftware (-1) (-1) yields Left (NegativeYears (-1))
--
-- If you recall from Chapter 17, we introduced a Validation datatype 
-- with an Applicative instance that was able to accumulate information 
-- regarding the various points of failure within a group of computations.
--
-- Because Applicative and Monads must have the same behaviour (we must 
-- be able to derive Applicative from Monad), it is not possible to have
-- a monadic instance for Validation. 


-- Short Exercise : Either Monad
--    Implement the Either Monad

data Sum a b = 
    First a 
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second y) = Second $ f y

instance Applicative (Sum a) where
  pure = Second 
  (<*>) (First x)   _          = First x
  (<*>) _           (First x)  = First x
  (<*>) (Second f)  (Second a) = Second $ f a

instance Monad (Sum a) where
  return = pure
  (>>=) (First x) _ = First x
  (>>=) (Second y) f = f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [ First <$> arbitrary
                    , Second <$> arbitrary ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

type SumSSI = Sum String (String, String, Integer)

sumCheckers :: IO ()
sumCheckers = do
  quickBatch $ functor (trigger :: SumSSI)
  quickBatch $ applicative (trigger :: SumSSI)
  quickBatch $ monad (trigger :: SumSSI)
    where trigger = undefined



-- Monad Laws
--
--  Identity 
--
--  m >>= return    = m           (right identity)
--  return m >>= f  = f x         (left  identity)
--
--  Basic premise is that 'return' should not perform any computation.
--
--
--  Associativity 
--
--  (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
--
--  Regrouping functions should not have any impact on the final result. 
--  
--
--  Monadic Composition
--  
--  Due to the layers of structure, we cannot compose monadic functions 
--  in a way similar to Functors or Applicatives. 
--
--  This will not typecheck:
--    mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
--    mcomp f g a = f (g a)
--
--  Very simply, because g is passing an (m b) to f (which is expecting
--  a value of type b), there is a layer of monadic structure getting 
--  in the way. What we want is to ignore that monadic context, and get
--  at the value encapsulated. 
--
--  This typechecks, and works as expected:
--    mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
--    mcomp' f g a = join (f <$> (g a))
--                 = g a >>= f
--
--  It is helpful to note that the Monad module already has an 
--  implementation of this. 
--
--  (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
--  g >=> f a == g a >>= f 
--
--
sayHi :: String -> IO String
sayHi greeting = do 
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

--readM' :: String -> IO Integer
--readM' = return . read

--testKleisliFish = sayHi >=> readM'

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"
--
--
--
--
-- Example implementation of a monad & associated derivative algebra
data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where 
  fmap f (CountMe i a) = CountMe i $ f a

instance Applicative CountMe where
  pure = CountMe 0
  (<*>) (CountMe n f) (CountMe n' a) =
    CountMe (n + n') $ f a

instance Monad CountMe where
  return = pure
  (>>=) (CountMe n a) f = 
    let CountMe n' b = f a -- f :: a -> CountMe b
     in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where 
  (=-=) = eq

countMeCheckers = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger






-- Chapter Exercises : 
--
-- Write Monad instances for the following types. Use the Checkers 
-- library to validate your instances

-- 1)
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where 
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) NopeDotJpg _ = NopeDotJpg

instance Monad Nope where
  (>>=) NopeDotJpg _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = 
    let f :: a -> Nope a
        f = const NopeDotJpg 
    in f <$> arbitrary 

instance EqProp (Nope a) where 
  (=-=) = eq

nopeCheckers :: IO ()
nopeCheckers = do
  let trigger :: Nope (String, String, Int)
      trigger = undefined 
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


-- 2)

data BahEither b a = 
    PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft $ f a

instance Applicative (BahEither b) where
  pure = PLeft  
  (<*>) (PRight x) _ = PRight x
  (<*>) _ (PRight x) = PRight x
  (<*>) (PLeft f) (PLeft x) = PLeft $ f x

instance Monad (BahEither b) where
  (>>=) (PLeft x) f = f x
  (>>=) (PRight x) _ = PRight x  

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = oneof [ PLeft <$> arbitrary
                    , PRight <$> arbitrary ]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

bahCheckers :: IO ()
bahCheckers = do 
  let trigger :: BahEither String (String, String, Integer)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


-- 3)
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

identityCheckers :: IO () 
identityCheckers = do 
  let trigger :: Identity (String, String, Integer)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


-- 4)
data List a =
    Nil
  | Cons a (List a) 
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) Nil x = x
  (<>) x Nil = x
  (<>) (Cons a b) xy@(Cons _ _) = Cons a (b <> xy)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) lst@(Cons _ _) = (f <$> lst) <> (fs <*> lst) 

instance Monad List where
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [ Cons <$> arbitrary <*> arbitrary
                    , return Nil]

instance Eq a => EqProp (List a) where
  (=-=) = eq

listCheckers :: IO () 
listCheckers = do 
  let trigger :: List (String, String, Integer)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


