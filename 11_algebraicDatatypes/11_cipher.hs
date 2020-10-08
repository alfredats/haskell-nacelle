module Vignere where

import Data.Char 

deltaOrd :: Char -> Int
deltaOrd x
  | isUpper x = (ord x) - (ord 'A')
  | isLower x = (ord x) - (ord 'a')
  | otherwise = error "deltaOrd : alpha chars only"

shift :: Int -> Char -> Char
shift num x
  | isLower x = chr . (+97) . newplace $ x  
  | isUpper x = chr . (+65) . newplace $ x
  | otherwise = error "shift : alpha chars only"
  where newplace = flip mod 25 . (+ num) . deltaOrd

numGen :: String -> Int -> Int
numGen str count = deltaOrd $ str !! (mod count (length str))

kpShift :: String -> String -> Int -> String
kpShift _ [] _ = []
kpShift kp (x:xs) count
  | not (isAlpha x) = x : kpShift kp xs count
  | otherwise = shift (numGen kp count) x : kpShift kp xs (count+1)

kpCheck :: String -> Bool
kpCheck [] = False
kpCheck (x:[]) = isAlpha x
kpCheck (x:xs) = isAlpha x && kpCheck xs


vignere :: String -> String -> String
vignere keyphrase text
  | not (kpCheck keyphrase) = error "keyphrase must be alphanumeric"
  | otherwise = kpShift keyphrase text 0
