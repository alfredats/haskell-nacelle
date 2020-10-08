module Newtypes where

-- Suppose we have a function as follows
tooManyGoats1 :: Int -> Bool
tooManyGoats1 n = n > 42


-- If we want to restrict the use of the function to goats only,
-- we can choose to define newtypes as follows.
newtype Goats = Goats Int deriving (Eq, Show)


-- Rewriting our function to be safer,
tooManyGoats2 :: Goats -> Bool
tooManyGoats2 (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 42

-- defining a type synonym for comparison
type Cows = Int 

-- This fails because instance declarations cannot use type synonyms
instance TooMany Cows where
  tooMany n = n > 42
