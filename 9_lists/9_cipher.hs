module Cipher where

import Data.Char

shift :: Int -> Char -> Char
shift x = chr . (+97) . flip mod 26 . (subtract 97) . (+x) . ord

caesar:: Int -> String -> String
caesar _ [] = []
caesar num (' ':xs) = ' ' : caesar  num xs
caesar num (x  :xs) = shift num x : caesar num xs

unCaesar num = caesar (-num)
