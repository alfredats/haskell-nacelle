data DayOfWeek = 
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Eq, Ord, Show)

-- Key points:
--  * Values to the left are "less than" values to the right when using
--  derived Ord instances.


-- In this example we define an Ord instance such that Friday is the
-- best day, and every other day is the same.
data MyDayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
deriving (Eq, Show)

instance Ord MyDayOfWeek where
    compare Fri Fri = EQ
    compare Fri _   = GT
    compare _ Fri   = LT
    compare _ _     = EQ
