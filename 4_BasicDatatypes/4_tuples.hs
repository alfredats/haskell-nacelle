-- Tuples
--
-- Tuples enable us to store multiple values in a single composite
-- value. They are referred to by the number of values (which does not 
-- have to be the same type) in each tuple.

-- The '(,)' constructor constructs a 2-tuple. '(,,)' constructs a
-- 3-tuple and so on. For info, use ':info (,)'

-- Tuples are a product type, as compared to the Bool data constructor
-- we saw before which was a sum type. The difference is that a product
-- type represents a logical conjuction, which in the case of the
-- 2-tuple means that the first AND (therefore logical conjunction)
-- second arguments must be provided. 
--
-- We can choose to provide two different type variables to the 2-tuple, 
-- but they don't necessarily have to be.

-- Tuple functions
--
-- Convenience functions for extracting the first or second values of
-- the 2-tuples are 'fst' and 'snd' (see ':info fst' for more info)
--
-- There are some other tuple functions defined in 'Data.Tuple', but we
-- have to import it.
--
-- Example:
-- import Data.Tuple
-- :info swap
--
-- Apparently it is best practice to use tuples only up to 5-tuples.


-- Here are some ways we can use tuples
--
-- Pattern matching example:
--  tupPatt :: (Int, [b])
--          -> (Int, [b])
--          -> (Int, [b])
--  tupPatt (a,b) (c,d) =
--      ((a + c), (b ++ d))
--
