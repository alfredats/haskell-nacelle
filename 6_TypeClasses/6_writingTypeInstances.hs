-- In writing instances of typeclasses for datatypes, refer to Hackage 
-- documentation of typeclass in question. There should be a note within
-- the documentation stating "Minimal complete definition:"
--
-- This tells us what methods have to be defined to have a valid
-- typeclass instance. 
--
-- In the case of 'Eq',
--      "Minimal complete definition:
--          either == or /-"

------------------------------------------------------

data Trivial = Trivial'

instance Eq Trivial where 
    Trivial' == Trivial' = True

------------------------------------------------------

data DayOfWeek =
    Mon | Tue | Wed | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
-- to check for non-exhaustive pattern matching, turn on all warnings
-- with ':set -Wall' in Prelude before loading the file.
    (==) _   _   = False

instance Show DayOfWeek where
    show Mon = "Mon"
    show Tue = "Tue"
    show Wed = "Wed"
    show Thu = "Thu"
    show Fri = "Fri"
    show Sat = "Sat"
    show Sun = "Sun"

data Date = 
    Date DayOfWeek Int 

instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
        weekday == weekday' 
     && dayOfMonth == dayOfMonth'



-- HOW DO I DO DIS
--  * Is it possible to to chain functions? 
--  * Do i create a seperate function that is of type IO?
--  * I've tried $, . and parenthesising each of the expressions... 


instance Show Date where
    show (Date weekday dayOfMonth) = 
        (show weekday) ++ " " ++ (show dayOfMonth)

-----------------------------------------------------------

-- | Datatypes that require parameters
--      * Datatypes that take polymorphic parameters require arguments
--      to provide typeclass instances when writing an instance for the
--      datatype
--      * Can be enforced by adding type class constraint syntax for
--      instance declarations

data Identity a = Identity a

instance Eq a => Eq (Identity a) where 
    (==) (Identity v) (Identity v') = v == v'

-----------------------------------------------------------

-- | Type Specificity
--      * There exists type defaults for functions that implement
--      typeclass constraints (re: Haskell Report type defaults for
--      numerical computations)
--      * If polymorphic values are used without typeclass constraints,
--      and no default rule is available, GHC complains.
--      * We can declare monomorphic functions from polymorphic
--      functions, but not the converse


