-- Exercise: Grab Bag
--  1) a & d
--  2) d
--  3)  a) f = (\n -> n + 1)
--      b) addFive = (\x -> \y  -> (if x > y then y else x) + 5)
--      c) mflip f y x = f y x

-- Exercise: Variety pack
--  1) a) k :: (a,b) -> a
--     b) k2 :: String. It is not the same type as k1 or k3.
--     c) Both k1 and k2
--  
--  2) f (a,b,c) (d,e,f) = ((a,d), (c,f))

-- Exercise: Case Practice
--  1)
functionC x y = 
  case (x > y) of 
    True  -> x
    False -> y

--  2)
ifEvenAdd2 n =
  case (even n) of
    True  -> n + 2
    False -> n

--  3)
nums x = 
  case res of
    LT -> -1
    GT -> 1
    EQ -> 0
  where res = compare x 0
--


-- Exercise: Artful Dodgy
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

--  1) Takes the first argument and sums it with the product of the 
--     second argument & 10
--  2)  11
--  3)  22
--  4)  21
--  5)  12
--  6)  11
--  7)  21
--  8)  21
--  9)  22
--  10) 31
--  11) 23


-- Exercise: Guard Duty
-- 1)
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | otherwise = 'F'
  | y >= 0.9  = 'A'
  where y = x / 100
-- 1) avgGrade 99 returns 'F' because of the otherwise which returns 
--    True for everything
-- 2) avgGrade 90 returns 'C'. Gotta start with most restrictive 
--    conditions
-- 3) b
-- 4 & 5) pal :: Eq a => [a] -> Bool
-- 6) c
-- 7 & 8) numbers :: (Ord a, Num a, Num p) => a -> p [NOTE: the input 
-- and output are technically two different num spaces]



-- Chapter Exercises: Multiple Choice
-- 1) d
-- 2) b 
-- 3) d 
-- 4) b 
-- 5) a 


-- Chapter Exercises: Let's write code
tensDigit :: Integral a => a -> a 
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10 
-- 1a & b)
myTensDigit :: Integral a => a -> a
myTensDigit = snd . (flip divMod 10) . fst . (flip divMod 10)

-- 1c)
hunsD :: Integral a => a -> a
hunsD = snd . (flip divMod 10) . fst . (flip divMod 100)


-- 2)
foldBoolCASE :: a -> a -> Bool -> a
foldBoolCASE x y bool = 
  case tf of 
    True -> y 
    False -> x
  where tf = bool

foldBoolGUARD :: a -> a -> Bool -> a
foldBoolGUARD x y bool
  | bool = y
  | otherwise = x 

-- 3)
g :: (a -> b) -> (a, c) -> (b, c)
g atob (a,c) = (atob a, c)

-- 4,5 & 6)
-- refer to 7_read.hs


