module Phone where

import Data.Char
-- 1) Create a data structure that captures the phone layout described

-- This data structure should be able to express enough of how the 
-- layout works so it can be used to dictate behavior of functions in
-- in later exercises.

type Digit = Char
type DigitStr = String 
type Presses = Int

data DaPhone = DaPhone [(Digit, DigitStr)] deriving Show

myPhone :: DaPhone
myPhone = DaPhone [ ('1', "1")
                  , ('2', "abc2")
                  , ('3', "def3")
                  , ('4', "ghi4")
                  , ('5', "jkl5")
                  , ('6', "mno6")
                  , ('7', "pqrs7")
                  , ('8', "tuv8")
                  , ('9', "wxyz9")
                  , ('*', "^")
                  , ('0', "+ _0")
                  , ('#', ".,") ]

-- 2) Convert the following conversations into keypresses required to 
--    express them.
convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol OK. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "OK. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"]


-- Consider the following dataflow for rf 
--  rf 'A' ('1', "") (('2',"abc2") : []) =
--  ('*', 1) : (rf 'a' ('1', "") (('2',"abc2") : [])) =
--  ('*', 1) : (rf 'a' ('2', "abc2") ([])) =
--  ('*', 1) : ('2', 1) : [] 

getPos :: Char -> DigitStr -> Presses
getPos c ks = go c ks (1 :: Presses)
  where
    go :: Char -> DigitStr -> Presses -> Presses
    go _ [] _ = 0
    go ch (x:xs) acc
      | x == ch = acc
      | otherwise = go ch xs (acc+1)

rf :: Char -> (Digit, DigitStr) -> [(Digit,DigitStr)] -> [(Digit, Presses)]
rf c tup [] = (fst tup, getPos c (snd tup)) : []
rf c tup (x:xs)  
  | getPos c (snd tup) == 0 = rf c x xs
  | otherwise   = (fst tup, getPos c (snd tup)) : [] 

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone dp) c 
  | isUpper c = ('*', 1) : rf (toLower c) (head dp) (tail dp) 
  | otherwise   = rf c (head dp) (tail dp)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone str = concatMap (reverseTaps phone) str


-- 3) How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps [] = 0
fingerTaps (x:xs) = (snd x) + fingerTaps xs


-- 4) What is the most popular letter for each message? What was its cost?

-- I took this from Chapter 10's exercises hahahha
tupMax :: Ord b => [(a, b)] -> (a, b)
tupMax xs = 
  foldr f (last xs) xs 
    where
      f = (\x y -> if (compare (snd x) (snd y) == GT) then x else y) 

-- This i adapted from the insertWith function in Data.Strict.Map
insertList :: (Ord a, Num b) => (b -> b -> b) 
   -> (a, b) -> [(a, b)] -> [(a, b)]
insertList _ (ch, num) [] = [(ch, num)]
insertList f (ch, num) (x:xs) = 
  case compare ch (fst x) of 
    LT -> (ch, num) : x : xs 
    GT -> x : (insertList f (ch, num) xs)
    EQ -> (ch, num + snd x) : xs

letterCounts :: String -> [(Char, Int)]
letterCounts = foldr (insertList (+)) [] . map (\x -> (x,1)) 


mostPopularLetter :: String -> (Char,Presses)
mostPopularLetter = tupMax . letterCounts 

-- 5) what is the most popular letter overall? what is the most popular 
--    word?
coolestLtr :: [String] -> Char
coolestLtr = fst . mostPopularLetter . foldr (++) ""

coolestWord :: [String] -> String
coolestWord = fst . tupMax . wordCounts . foldr ((++) . words) [] 
  where 
    wordCounts = foldr (insertList (+)) [] . map (\x -> (x,1))












-- Printing all results [TESTS]
stringToTaps :: String -> IO ()
stringToTaps []  = putStr ""
stringToTaps str = 
  do
    putStrLn ("Message: " ++ str)
    putStrLn ("Tap Sequence required: " ++ show tabSeq)
    putStrLn ("Total Taps required: " ++ show (fingerTaps tabSeq))
    putStrLn ("Most Popular Letter: " ++ show (mostPopularLetter str))
    putStrLn ""
  where tabSeq = cellPhonesDead myPhone str

convoToTaps :: [String] -> IO ()
convoToTaps = mapM_ stringToTaps 

main :: IO ()
main = do
  convoToTaps convo
  putStrLn ("Overall most popular letter: " ++ show (coolestLtr convo))
  putStrLn ("Overall most popular word: " ++ show (coolestWord convo))
