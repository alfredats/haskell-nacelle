-- Intermission Exercise 8.2
--
-- applyTimes 5 (+1) 5 = (+1) . (+1) . (+1) . (+1) . (+1) $ 5
--                     = (+1) . (+1) . (+1) . (+1) $ 6
--                     = ...
--                     = (+1) $ 9
--                     = 10
--

-- Chapter Exercise
--    Types review
--      1) d
--      2) b
--      3) d
--      4) b
--
--
--    Currying review
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow" ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String 
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

--      1)  "woops mrowwoohoo!"
--      2)  "1 mrowhaha"
--      3)  "woops mrow2 mrowhaha"
--      4)  "woops mrowblue mrowhaha"
--      5)  "prink mrowhaha mrowgreen mrowwoops mrowblue"
--      6)  "are mrowPugs mrowawesome"


--    Recursion
--      1) dividedBy 15 2 = go 15 2 0
--                        = go 13 2 1 
--                        = go 11 2 2
--                        = ...
--                        = go 3 2 6
--                        = go 1 2 7
--                        = (7,1)
--      2)
recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum x
  | x == 1 = 1
  | otherwise = 
      x + recursiveSum (x-1)

--      3)
integralMult :: (Integral a) => a -> a -> a
integralMult x y = go x 0 
  where go x cumsum
          | x == 0 = cumsum  
          | x > 0 = go (x - 1) (cumsum + y)
          | x < 0 = go (x + 1) (cumsum - y)


--    Fixing dividedBy
--      refer to 8_division.hs

--    McCarthy 91
mcCarthy91 :: (Ord a, Num a) => a -> a 
mcCarthy91 x 
  | x > 100 = x - 10
  | otherwise = 91



--      Numbers into Words
--        refer to 8_wordNumber.hs
