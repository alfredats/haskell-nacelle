module Ciphers where

import Data.Char
import System.IO

-- Basic Helpers
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

-- Caesar Cipher 
caesar:: Int -> String -> String
caesar _ [] = []
caesar num (' ':xs) = ' ' : caesar  num xs
caesar num (x  :xs) = shift num x : caesar num xs

caeCipher :: IO ()
caeCipher = do
  hSetBuffering stdout NoBuffering
  putStr "Enter the text to be codified: "
  txt <- getLine
  putStr "Enter the number of shifts to be used: "
  shft <- getLine
  case all isNumber shft of
    True  -> putStrLn (caesar (read shft::Int) txt)
    False -> putStrLn "Shifts must be integer values"

-- Vignere Cipher
numGen :: String -> Int -> Int
numGen str count = deltaOrd $ str !! (mod count (length str))

kpShift :: String -> String -> Int -> String
kpShift _ [] _ = []
kpShift kp (x:xs) count
  | not (isAlpha x) = x : kpShift kp xs count
  | otherwise = shift (numGen kp count) x : kpShift kp xs (count+1)

vignere :: String -> String -> String
vignere keyphrase text
  | not (all isAlpha keyphrase) = error "keyphrase must be alphanumeric"
  | otherwise = kpShift keyphrase text 0

vigCipher :: IO () 
vigCipher = do
  hSetBuffering stdout NoBuffering
  putStr "Enter the text to be codified: "
  txt <- getLine
  putStr "Enter the keyphrase to be used: "
  kyphrs <- getLine
  putStrLn (vignere kyphrs txt)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Which cipher would you like to use? (Caesar/Vignere)"
  cphr <- getLine
  case cphr of
    "Caesar" -> caeCipher
    "Vignere" -> vigCipher
    _ -> main
