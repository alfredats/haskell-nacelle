-- For More Bottoms exercise 6
import Data.Bool
import Data.Char
-----------------------------------------

-- Exercise: Enum From To
--    refer to 9_enumFromTo.hs


-- Exercise: Thy fearful symmetry
--  1)

-- These 2 versions work
myWords'' :: String -> [String]
myWords'' "" = []
myWords'' (' ':xs) = myWords xs
myWords'' xs = (takeWhile (/=' ') xs) : myWords'' (dropWhile (/=' ') xs)

myWords' :: String -> [String]
myWords' [] = []
myWords' (x:xs)
  | xs == "" = []
  | x == ' ' = myWords' xs
  | otherwise = [takeWhile (/=' ') (x:xs)] ++ 
      (myWords' . dropWhile (/=' ') $ xs)


-- The following version are not accurate
-- test using :
--    myWords  " sheryl had fun"

myWords :: String -> [String]
myWords str = go str []
  where
    go [] _ = []
    go (y:ys) strlist
      | ys == ""  = strlist
      | y == ' ' = myWords ys
      | otherwise = 
          go (dropWhile (/=' ') (y:ys)) (strlist ++ 
            [takeWhile (/=' ') (y:ys)])  


-- 2)
--    refer to 9_myLines.hs

-- 3)
msb :: Char -> String -> [String]
msb c str = go str
  where 
    go [] = []
    go (x:xs)
      | (x:xs) == "" = []
      | x == c = go xs
      | otherwise = 
          (takeWhile (/=c) (x:xs)) : (go . dropWhile (/=c) $ (x:xs))



-- Exercise: Comprehend thy lists
-- Given mySqr = [x^2 | x <- [1..10]] = [1,4,9,16,25,36,49,64,81,100],

-- [x | x <- mySqr, rem x 2 = 0] = [4,16,36,64,100]
--
-- [(x,y) | x <- mySqr,
--          y <- mySqr,
--          x < 50, y > 50] = [(1 ,64), (1 ,81), (1 ,100),
--                             (4 ,64), (4 ,81), (4 ,100),
--                             ...
--                             (49,64), (49,81), (41,100)]
--
-- take 5 [ (x,y) | x <- mySqr,
--                  y <- mySqr,
--                  x < 50, y > 50] = [(1 ,64), (1 ,81), (1 ,100),
--                                     (4 ,64), (4 ,81)]



-- Exercise: Square Cube
-- Given 
mySqr  = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- 1)
list1 = [(x,y) | x <- mySqr, y <- myCube]
-- 2)
list2 = [ a | a <- list1, fst a < 50, snd a < 50]
-- 3)
mylen :: [a] -> Integer
mylen [] = 0
mylen (_:xs) = 1 + mylen xs

mylen' xs = length xs



-- Exercise: Bottom Madness
-- 1) Returns bottom because one of the values is a bottom (undefined).
-- 2) Returns a value. Strict evaluation only occurs before the bottom is 
--    reached.
-- 3) Returns bottom due to the undefined value within the list (note: 
--    sum is value-strict)
-- 4) Returns a value. Length is spine-strict and ignores the bottom value
-- A
-- 5) Returns bottom. Even if length is spine-strict, (++) is value-strict.
-- 6) Returns a value. Despite the value-strict nature of (even), (take)
--    only requires the strict value-evaluation of the list up to the
--    first value. This prevents the bottom from being reached.
-- 7) Returns bottom. The list is traversed fully due to earlier elements 
--    not being able to fulfill (even), causing the bottom type to be
--    encountered by the value-strict (even) function. 
-- 8) Returns value. Same as (6), but for odd
-- 9) Returns value. Same as (8)
--10) Returns bottom. Same as (7), but with (odd)



-- Intermission: Is it in normal form?
-- 1) Normal Form. There is no further evaluation.
-- 2) Weak Head Normal Form. The list is awaiting a data constructor
-- 3) Neither. The expression is a function awaiting evaluation.
-- 4) Neither
-- 5) Neither.
-- 6) Neither. (++) is awaiting evaluation.
-- 7) Weak Head Normal Form. It is a tuple constructor awaiting input 
--    from a data constructor.  ** I'm not 100% sure about this**



-- Exercise: More bottoms
--  1) Bottom.
--  2) [2] 
--  3) Bottom.
--  4) The function has type '[Char] -> [Bool]'. It takes a string, and
--     checks element-by-element if it is a vowel. It then returns a list
--     of boolean values, indexed according to the characters of the
--     string, as to if the character in the corresponding position of 
--     the string is a vowel. 
--  5) a) [1,4,9,16,25,36,49,64,81,100]
--     b) [1,10,20]
--     c) [15,15,15]
--  6) 
q6 = map (\x -> bool x (-x) (x==3)) [1..10]



-- Exercise: Filtering
-- 1)
f1 = [x | x <- [1..30], rem x 3 == 0]
f1' = filter (\x -> rem x 3 ==0) [1..30]

-- 2)
f2 = length . filter (\x -> rem x 3 == 0) 
-- 3)
myFilter :: String -> [String]
myFilter [] = []
myFilter xs = go . words $ xs
  where 
    go []         = []
    go ("the":ys) = go ys
    go ("a"  :ys) = go ys
    go ("an" :ys) = go ys
    go (y:ys)     = y :go ys 



-- Exercise: Zipping
-- 1)
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y): myZip xs ys
-- 2)
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = (f x y): myZipWith f xs ys
-- 3)
myZip' :: [a] -> [b] -> [(a,b)]
myZip' = myZipWith (,) 



-- Chapter Exercises

-- Data.Char
-- 1) isUpper :: Char -> Bool
--    toUpper :: Char -> Char
-- 2) 
filterUpper :: String -> String
filterUpper = filter isUpper -- alternatively use (not . isLower)
-- 3)
myCap :: String -> String
myCap [] = []
myCap (x:xs) = toUpper x : xs
-- 4)
myCapAll :: String -> String
myCapAll [] = []
myCapAll (x:xs) = toUpper x : myCapAll xs
-- 5) 
capOnly :: String -> Char
capOnly []    = error "Empty List"
capOnly (x:_) = toUpper x
-- 6)
capOnly' :: String -> Char
capOnly' = toUpper . head 


-- Cipher
--    refer to 9_cipher.hs


-- Writing your own standard functions
--    refer to 9_standard.hs

