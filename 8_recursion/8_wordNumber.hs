module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "Zero"
  | n == 1 = "One"
  | n == 2 = "Two"
  | n == 3 = "Three"
  | n == 4 = "Four"
  | n == 5 = "Five"
  | n == 6 = "Six"
  | n == 7 = "Seven"
  | n == 8 = "Eight"
  | n == 9 = "Nine"


digits :: Int -> [Int]
digits n = go n []
 where go n xs
        | n == 0 = xs
        | otherwise =
            go (div n 10) (concat [(mod n 10):[], xs])

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map digitToWord $ digits n
