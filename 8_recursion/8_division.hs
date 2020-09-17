module Division where

-- the type keyword is used to declare type aliases
-- contrasted to data constructors which use the "data" keyword
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy = div 

-- A more complete case (returns quotient & remainder)
dividedBy' :: Integral a => a -> a -> (a,a)
dividedBy' num denom = foo num denom 0
  where foo n d count
          | n < d = (count, n)
          | otherwise =
            foo (n-d) d (count + 1)


-- Fixing DividedBy

-- Complete version without having to create new datatype
dividedByFixed :: Integral a => a -> a -> (a,a)
dividedByFixed num denom = go num denom 0 
  where go n d count
          | d == 0 = error "Division by zero"  
          | d < 0 = go (-n) (-d) 0
          | (abs n) < d = (count,n)
          | n > 0 = go (n-d) d (count + 1)
          | n < 0 = go (n+d) d (count - 1) 

-- Complete version with new datatype
data DividedResult =
    Result Integer
  | DividedByZero deriving Show

dividedByDT :: (Integral a) => a -> a -> DividedResult
dividedByDT num denom = go num denom 0 
  where go n d count
          | d == 0 = DividedByZero
          | d < 0 = go (-n) (-d) 0
          | (abs n) < d = Result count
          | n > 0 = go (n - d) d (count + 1)
          | n < 0 = go (n + d) d (count - 1) 
