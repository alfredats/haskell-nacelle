-- Comparing Values

-- not equals is written as '/='

-- Special types for comparison operators
--  1) '==' uses the type 'Eq', '<' and the such use type 'Ord'
--  2) 'Eq' includes everything that can be compared and determined to
--      be equal in value
--  3)  'Ord' includes all things that can be ordered
--  4)  Both 'Eq' and 'Ord' are not restricted to numbers, and can
--      include letters and ths such.

--  With chars, 'a' is greater than 'A'. (Why? Is it because of ascii or
--  unicode?) 

--  A datatype with no instance of Ord will not work with '<' etc.
data Mood = G | B deriving Show
[ G, B ] > [B, G]
-- This will return an error stating 'No instance for (Ord Mood)
-- arising...'. This means that 'Mood' doesn't have an 'Ord' instance,
-- so the type checker doesn't know how to order values of that type.


-- One thing to remember: Numeric values are polymorphic, and the type
-- checker will try to resolve comparisons with values of other types. 
--
-- Example:
8/1 == 8
-- LHS is Rational. Since the '==' operator necessitates both LHS & RHS
-- have the same type (see ':t (==)'), the type checker will resolve RHS
-- to type Rational




