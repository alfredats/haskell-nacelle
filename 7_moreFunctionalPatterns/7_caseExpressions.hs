module CaseExpressions where

-- Checks if x + 1 == 1
funcZ x =
  case x + 1 == 1 of
    True  -> "Awesome!"
    False -> "wut"

-- Checks if xs is a palindrome
pal xs =
  case xs == reverse xs of
    True  -> "Yes"
    False -> "No"

pal' xs = 
  case y of
    True  -> "Yes"
    False -> "No"
  where y = xs == reverse xs


