{-# LANGUAGE InstanceSigs #-}
module Chapter23 where

import Control.Monad.Trans.State

import Control.Applicative (liftA3)
import Control.Monad
import System.Random
import Data.Monoid
import qualified Data.DList as DL

-- State
--
--  A light switch has two possible states, either on or off. In 
--  Haskell, when we wish to encapsulate the idea and convenience of 
--  a value that potentially changes with each computation (without 
--  resorting to mutability of data), we use the State type. 
--
--  The State type has the following advantages:
--    1. doesn't require IO
--    2. only affects data within the State container
--    3. maintains referential transparency
--    4. is explicit in the types of functions
--
--
-- The State newtype
--
newtype State' s a = State' { runState' :: s -> (a, s) }
-- 
--  Notice the similarity to the Reader newtype. Recall that newtypes 
--  are wrappers of types, and disappear at compile time. Therefore, the
--  function contained within the newtype must be isomorphic to the type
--  it wraps i.e. there must be a way to go from the newtype to the 
--  type it wraps and back again without a loss of information.
--
--    These are not isomorphic types:
--      1.  a -> Maybe b
--          
--          The possibility of returning Nothing causes a loss of 
--          information. It is not possible to know what value of a 
--          resulted in the Nothing value when working backwards.
--
--      2.  [a] -> a
--
--          This is not an isomorphism because firstly, whenever there 
--          is more than one element in [a], it is not possible to 
--          deconstruct the summary output value to the input list. 
--          Secondly, [a] -> a is a partial function because there might 
--          not be any elements in the list.
--
--
--    This is an isomorphic type:
--      1.  a -> Sum a
--        
--          The bijective nature of the function allows us to transform
--          from to the Sum newtype and back (with the getSum function).
--
--  
--
--  State takes an input state and returns an output value tupled with 
--  the new state value. The previous state value from each application
--  is chained to the next one in a pure way i.e. there are no effects 
--  on computation. The polymorphism of State also allows us to 
--  transform the output values as required.
--
--  The System.Random library utilizes a datatype very similar to State,
--  so let's take a closer look at it.
--
--
-- Throw Down
--  
--  In this section we use System.Random to simulate a dice.
data Die = 
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of 
               1 -> DieOne
               2 -> DieTwo
               3 -> DieThree
               4 -> DieFour
               5 -> DieFive
               6 -> DieSix
               -- Don't use error in general, it's only here to provide
               -- a quick way for the function to fail
               x -> error $ "inttoDie only takes integer 1 - 6:" 
                      ++ show x

-- This function is pure  i.e. given the same initial conditions 
-- provided by mkStdGen 0, it always produces the same result
rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _ ) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)


-- We can refactor the above function with State
rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

-- shorter
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))


rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie


--  to use rollDieThreeTimes', we have to give it an initial state to 
--  work with. This is due to the partially applied randomR function in
--  the state transformers within rollDie
--    
--    > evalState rollDieThreeTimes' (mkStdGen 0)
--    (DieSix, DieSix, DieFour)
--    > evalState rollDieThreeTimes' (mkStdGen 1)
--    (DieSix, DieFive, DieTwo)
--
--
--  If we try to generate a list of Die values however,
--    
--    repeat :: a -> [a]
--    infiniteDie :: State StdGen [Die]
--    infiniteDie = repeat <$> rollDie
--
--    gen = mkStdGen 0
--    take 4 $ evalState infiniteDie gen
--    > [DieSix, DieSix, DieSix, DieSix]
--
--  What's happening here is that a single die value is repeated, 
--  instead of repeating the state action. To fix this, we need to 
--  repeat the monadic stdGen using replicateM.
--
--    replicateM :: Monad m => Int -> m a -> m [a]
--    nDie :: Int -> State StdGen [Die]
--    nDie n = replicateM n rollDie
--
--    evalState (nDie 5) (mkStdGen 0)
--    > [DieSix, DieSix, DieFour, DieOne, DieFive]
--
--  
--  In this next example, we roll a single die until we reach or exceed
--  a sum of 20. 
--
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen 
          | sum >= 20 = count
          | otherwise = 
              let (die, nextGen) = randomR (1, 6) gen
              in go (sum + die) (count+1) nextGen


-- Exercises : Roll your own
--
--  1.  Refactor rollsToGetTwenty such that the limit is an argument 
--      to the function
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen 
          | sum >= n = count
          | otherwise = 
              let (die, nextGen) = randomR (1, 6) gen
               in go (sum + die) (count + 1) nextGen


-- 2. Change rollsToGetN to record the series of dice that are rolled,
--    in addition to the count of the total number of rolls.
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged  n g = go [] 0 g
  where go :: [Int] -> Int -> StdGen -> (Int, [Die])
        go lst count gen 
          | getSum (foldMap Sum lst) >= n = (count, map intToDie lst)
          | otherwise = 
              let (die, nextGen) = randomR (1, 6) gen
               in go (die: lst) (count + 1) nextGen


-- Write State for yourself
--
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b  
  fmap f (Moi g) = Moi $ \s -> (,) <$> (f . fst . g) <*> id $ s 
-- (,) <$> (f . fst . g) <*> (snd . g)
-- \s -> ((f . fst . g) s, (snd . g) s) 

f = (+1) <$> (Moi $ \s -> (0, s))
testFunc :: IO ()
testFunc = print $ runMoi f 0


instance Applicative (Moi s) where
  pure  :: a -> Moi s a 
  pure a = Moi $ \s -> (a, s)
  (<*>) ::  Moi s (a -> b) -> Moi s a -> Moi s b
  (<*>) (Moi f) (Moi g) = Moi $ \s -> ((fst $ f s) (fst . g $ s) , s)


instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (>>=) (Moi f) g = Moi $ \s -> (fst (runMoi (g . fst . f $ s) s), s)



-- FizzBuzz
--  Write a program that prints the numbers from 1 to 100. But, for 
--  multiples of 3 print "Fizz", and for multiples of 5 print "Buzz".
--  For multiples of both three and five print "FizzBuzz".

--  Typical Fizzbuzz soln:
fizzBuzz :: Integer -> String
fizzBuzz n 
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5  == 0 = "Buzz"
  | n `mod` 3  == 0 = "Fizz"
  | otherwise       = show n


fb1 :: IO ()
fb1 = mapM_ (putStrLn . fizzBuzz) [1..100]


-- FizzBuzz using State
fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do 
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fb2 :: IO ()
fb2 = mapM_ putStrLn $ reverse $ fizzBuzzList [1..100]

--  This implementation is not quite inefficient due to the reversal 
--  operation in fb2. Reversing singly-linked lists is typically not 
--  great, and also it won't terminate given an infinite list.
--
--  One way we could handle this is to utilize a data structure that
--  allows us to append elements to the end of the list. (++) is slow, 
--  so we can use something else called the Difference List that has 
--  an O(1) appending operation.

-- making an amendment using Difference Lists 
fizzBuzzList' :: [Integer] -> DL.DList String 
fizzBuzzList' list = 
   execState (mapM_ addResult' list) DL.empty

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result) -- snoc appends to the end instead of the 
                          -- front like cons

fb3 :: IO ()
fb3 = mapM_ putStrLn $ fizzBuzzList' [1..100]


-- FizzBuzz Differently
--  Fix our reversing FizzBuzz by changing the code in the following way
fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo x y = 
  let list = go x y []
      go :: Integer -> Integer -> [Integer] -> [Integer]
      go curr lim xs  
        | curr > lim = xs
        | otherwise = go (curr+1) lim (curr : xs)
   in execState (mapM_ addResult list) []



-- Chapter Exercises
--  
--  1.  Construct a state where the state is also the value u return
get' :: State' s s
get' = State' $ \s -> (s, s) 

specGet :: IO ()
specGet = print $ 
  runState' get' "curryIsAmaze" == ("curryIsAmaze", "curryIsAmaze") 


--  2.  Construct a state where the resulting state is the argument 
--      provided. 
put' :: s -> State' s ()
put' s = State' $ \m -> ((), s)

specPut :: IO ()
specPut = print $
  runState (put "blah") "woot" == ((), "blah")


-- 3. Run the State with s and get the state that results
exec' :: State' s a -> s -> s
exec' f@(State' _) s = snd $ runState' f s


-- 4. Run the State with s and get the value that results
eval' :: State' s a -> s -> a
eval' f@(State' _) s = fst $ runState' f s


-- 5. Write a function that appliesa  function to create a new state
fMod' :: (s -> s) -> State' s ()
fMod' f = State' $ \s -> ((), f s)
