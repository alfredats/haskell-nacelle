-- Numeric types


-- Integral numbers

--  Int: Fixed precision i.e. cannot support arbitrarily large/small
--      (minBound, maxBound) :: (Int, Int)

--  Integer: Can support arbitrarily large/small, up to local memory
--           limit
--      (minBound, maxBound) :: (Integer, Integer) will throw an error.

--  Word: Smallest number is zero, but same "range" as Int.
--      (minBound, maxBound) :: (Word, Word) 



-- Fractional numbers

--  Float: Single-precision floating point numbers

--  Double: Double-precision flaoting point numbers.

--  Fixed: Fixed-precision that represent varying number of decimal
--         points
--      'Fixed E2' tracks up to 2 digits after the decimal point
--      The 'base' library provides up to E12, can define if more needed 

--  Scientific: Almost arbitrary precision

--  Rational: 
--      1 / 2 :: Rational


-- Type class constraints

--  When defining functions, we might come across expressions like
--  'Integer a =>'. These are known as type class constraints, and tells 
--  us that whatever type variable 'a' turns out to be, it must have an 
--  instance of the Integer type.

--  Taking the type of the division function,
--  (/) :: Fractional a => a -> a -> a
--         ^^^^^^^^^^^^^^^
--  The / function will take one number that implements Fractional (as
--  restricted by the type class constraint), divide it by another
--  Fractional, and return a Fractional value.

-- (superclass is briefly mentioned here)


