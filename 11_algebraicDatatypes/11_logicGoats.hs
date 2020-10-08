{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

-- recall: a typeclass is a collection of methods, an instance of a 
--         typeclass defines how those methods are used for the specific
--         type.
class TooMany a where 
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- Without newtype
instance TooMany (Int,String) where
  tooMany (int, str) = tooMany int 

-- With newtype
newtype IntString = IntString (Int, String)
instance TooMany IntString where
  tooMany (IntString (int, _)) = tooMany int


-- Other instances
instance TooMany (Int, Int) where
  tooMany (xint, yint) = tooMany (xint + yint) 

instance (Num a, TooMany a) => TooMany (a,a) where
  tooMany (num, toomany) = tooMany (num + toomany) 
