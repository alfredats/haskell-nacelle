{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-------------------------------------------------
-- defining a typeclass, and instances.
-------------------------------------------------

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42



--------------------------------------------------
-- reusing instances in newtype definitions
-------------------------------------------------

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)
 -- we're allowed to reuse the instance of TooMany that was
 -- defined for Int, due to the language pragma called 
 -- GeneralizedNewtypeDeriving in the first line.
