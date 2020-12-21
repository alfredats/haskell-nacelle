{-# LANGUAGE InstanceSigs #-}

module Chapter22 where

import Control.Applicative

-- READER


-- Functorial contexts, and the Functor of Functions
--
--  boop :: Num a => a -> a
boop = (*2)
--  doop :: Num a => a -> a
doop = (+10)
--  bip :: Num a => a -> a
bip = boop . doop
--  bloop :: Integer -> Integer
bloop = fmap boop doop

--  `bloop` is equivalent to `bip`. The functorial context in bloop is a 
--  partially applied function, where typically it would be a datatype. 
--    
--    fmap boop doop x == (*2) ((+10) x)
--      or 
--    fmap boop doop = \x -> boop (doop x) 
--                   = fmap boop $ doop  [doop is awaiting application]
--
--
-- Applicative Contexts
--
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

--  In this example, we added another function to lift over the contexts
--  of partially applied functions. However, in this case, both boop &
--  doop are partially applied, and any argument is passed to both in
--  parallel. 
--
--  Let's look at bbop.
--    
--  Suppose we have
--    f :: Integer -> Integer
--    g :: Integer -> Integer
--
--  Then, looking at bbop,
--    sameAsBbop f g = (+) <$> f <*> g
--    
--  The (<*>) in sameAsBbop actually has the following type signature, 
--  with (Integer ->) as the Applicative f in (<*>)'s usual type sig.
--    
--    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
--
--    (<*>) :: (Integer -> Integer -> Integer) 
--          -> (Integer -> Integer) 
--          -> (Integer -> Integer)
--
--  The (Integer -> Integer -> Integer) above comes from ((+) <$> f) as
--  the (+) function is partially applied and still waiting an argument.
--
--    ((+) <$> f) :: Integer -> Integer -> Integer
--    (+) <$> f == (\x -> (+) (f x))
--
--  With g taking the second function argument to (<*>), we can now look
--  at sameAsBbop as:
--    
--    sameAsBbop = f' <*> g     where f' = (+) <$> f
--               = g'           with g' :: Integer -> Integer
--
--  Any input to (f' <*> g) is parallely applied to both f' and g. This 
--  is useful when two functions share the same input, and we want to 
--  apply some other function to their result to get a final output.
--
--  We can also do the same thing using a Monadic context.
boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

--  Remember that the functorial context here is ((->) a). Any input to 
--  boopDoop will be applied to both boop and doop. 
--
--  The concept we've been elucidating over the past few examples is the
--  premise for Reader. It provides a way of stringing functions, such 
--  that all the functions are awaiting a single input from a shared
--  environment. Using reader allows us to pass a single value as an 
--  argument to a set of functions.
--
--  
-- Short Exercise : Warming up
--  refer to 22_warmingUp.hs
--
--
--
-- Breaking Down the Functor of Functions
--
--    instance Functor ((->) r) where
--      fmap = (.)
--
--  We've shown above what fmap does with functions, but let's do a 
--  deeper dive. As demonstrated earlier with the boopDoop example, 
--  the functorial context here, or rather the structure being lifted 
--  is the ((->) a) of (a -> b). In this specifc usecase of lifting 
--  of functions , we write ((->) r) instead, with 'r' known as Reader.
--  
--  Specialized to function lifting,
--      fmap :: (a -> b) -> f a      -> f b        sub f for ((->) r)
--      fmap :: (b -> c) -> (r -> b) -> r -> c 
--  
--
--
-- But uh, Reader?
--  
newtype Reader r a =
  Reader { runReader :: r -> a }
--
--  Reader is a newtype wrapper for the function type. The runReader 
--  accessor gets the function our of the Reader wrapper. 
--
--    f = Reader (+1)
--    runReader f (1) 
--    > 2
--  
--  Reiterating a point about fmap and composition,
--
instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ \r -> f (ra r)
--    
--    compose f g = \x -> f (g x)
--
--  The functor instance of Reader, and function composition are 
--  equivalent.
--
--
--  Exercise : Ask
--    Implement the follwing function.
ask :: Reader a a
ask = Reader id
--
--
--  Demonstrating the Function Applicative
--
--  This example is similar to what we've seen in previous chapters, 
--  but aimed specifically at showcasing the Applicative of functions.

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName   = DogName   String deriving (Eq, Show)
newtype Address   = Address   String deriving (Eq, Show)

data Person = Person {
                humanName :: HumanName
              , dogName   :: DogName 
              , address   :: Address
                     } deriving (Eq, Show)

data Dog    = Dog   {
                dogsName    :: DogName
              , dogsAddress :: Address
                    } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") 
              (DogName   "Barkley" )
              (Address   "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName   "Papu")
               (Address   "Austin")

-- Without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- We can also use methods in Control.Applicative with Reader
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

--
--
--  Exercise : reading comprehension
--
--  1. Write liftA2 yourself
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f g h = f <$> g <*> h

--  2. Write the following function
asks :: (r -> a) -> Reader r a
asks f = Reader f

--  3. Implement the Applicative for Reader
instance Applicative (Reader r) where
  pure :: a -> Reader r a 
  pure = Reader . const
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rab) (Reader ra) = 
    Reader $ \r -> rab r (ra r) 

-- 
--
-- The Monad of functions
--
--  As with the functor and applicative instances we've written so far,
--  functions also have a Monad instance. 
--
--    return  :: Monad m => a -> m a    -- it might be useful to recall
--    fReturn :: a -> r -> a            -- that (->) is right associative
--
--    (>>=)   :: Monad m => m a -> (a -> m b) -> m b
--    fMonad  :: (r -> a) -> (a -> r -> b) -> (r -> b)
--    
--  The instance of Monad for Reader is as follows 
--  (Implemented as, Exercise : Reader Monad)
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader ra) aRb = Reader $ \r -> runReader (aRb (ra r)) r

getDogRM :: Person -> Dog
getDogRM = do 
  name <- dogName 
  addy <- address
  return $ Dog name addy

getDogRM' :: Person -> Dog
getDogRM' = dogName >>=
              \name -> address >>=
                \addy -> return $ Dog name addy
--  
--
-- 
-- Changing Reader contexts
-- 
--  When we want to change swap in a different type or value of r for 
--  functions within Reader, we can use the following:
withReaderT :: (r' -> r) -> Reader r a -> Reader r' a
withReaderT f m = Reader $ runReader m . f
-- 
--  This style of using Reader to manipulate contexts i.e. the 'r', 
--  is called a monad Transformer. It is implemented in 
--  Control.Monad.Reader.
--
--
-- Chapter Exercises
--
--  Warm Up Stretch
--    refer to 22_ReaderPractice.hs
--
