-- Exercise: EnumFromTo

-- Basic function to use : 2 ways to write 
eftBasic :: (Enum a, Ord a) => a -> a -> [a]
eftBasic x y
  | x > y  = []
  | x == y = [x]
  | otherwise = (x : (eftBasic (succ x) y))


eftBasic' :: (Enum a, Ord a) => a -> a -> [a]
eftBasic' x y = go x y []
  where go x y xs
          | x > y   = xs
          | x == y  = xs ++ (x : [])
          | x < y   = go (succ x) y (xs ++ [x])


-- Explicit construction of eftBool
eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool False _    = [False]
eftBool True  _    = []

-- Using the Basic function
eftBool' :: Bool -> Bool -> [Bool]
eftBool' = eftBasic


-- Rest of the functions
eftOrdering :: Ordering -> Ordering -> [Ordering]
eftOrdering = eftBasic

eftInt :: Int -> Int -> [Int]
eftInt = eftBasic

eftChar :: Char -> Char -> [Char]
eftChar = eftBasic


