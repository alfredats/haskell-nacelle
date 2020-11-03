module Chapter14 where

import Data.Char
import Data.List 
import Test.QuickCheck
-- Chapter Exercises: 


-- Failure

--  Given
square x = x * x
squareIdentity = square . sqrt

prop_squareIdentity = 
  property $ (\c -> squareIdentity c == (c :: Float))
-- This fails because floating point representations are not 100% accurate 
-- which then results in the arithmetic having error propagation.


-- Idempotence

-- Taken from 11_exercises.hs
capitalizeWord :: String -> String
capitalizeWord (' ':xs) = ' ' : capitalizeWord xs
capitalizeWord (x:xs) = toUpper x : xs

-- Given
twice f = f . f 
fourTimes = twice . twice

f x = 
  (capitalizeWord x == twice capitalizeWord x) && 
  (capitalizeWord x == fourTimes capitalizeWord x)

f' x = 
  (sort x == twice sort x) &&
  (sort x == fourTimes sort x)

prop_capitalizeWord_idempotent :: Property
prop_capitalizeWord_idempotent =
  property $ (\str -> f (str :: String) == True)

prop_sort_idempotent :: Property
prop_sort_idempotent = 
  property $ (\str -> f' (str :: String) == True)


-- Making a Gen random generator

-- Given
data Fool = Fulse | Frue deriving (Eq, Show)

foolGen_equalProb :: Gen Fool
foolGen_equalProb = frequency $ [ (1, return (Fulse)) 
                                , (1, return (Frue)) ]

foolGen_moreFulse :: Gen Fool
foolGen_moreFulse = frequency $ [ (2, return (Fulse))
                                , (1, return (Frue)) ]

main :: IO () 
main = do
  putStrLn "Idempotence"
  quickCheck prop_capitalizeWord_idempotent
  quickCheck prop_sort_idempotent
