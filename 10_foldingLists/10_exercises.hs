-- foldr vs foldl 

-- This example should illustrate the characteristics of foldl & foldr
--
--    const :: a -> b -> a
--    const x _ = x
--    flip :: (a -> b -> c) -> b -> a -> c
--
--    foldl (flip const) 0 [1..5] = 
--      (((((0 `f` 1) 'f' 2) 'f' 3) 'f' 4) 'f' 5) =
--      (((( 1 'f' 2) 'f' 3) 'f' 4) 'f' 5) =
--      (((  2 'f' 3) 'f' 4) 'f' 5) =
--      ((   3 'f' 4) 'f' 5) =
--      (    4 'f' 5) = 5   (where `f` = flip const)
--
--  NOTE: In the above case, because of lazy evaluation, Haskell 
--        actually disposes of the 4 innermost brackets and returns 5
--        immediately. Nonetheless, it must traverse the spine and 
--        construct the relevant expressions before evaluation.
--
--    foldl const 0 [1..5] = 
--      (((((0 `g` 1) 'g' 2) 'g' 3) 'g' 4) 'g' 5) =
--      (((( 0 'g' 2) 'g' 3) 'g' 4) 'g' 5) =
--      (((  0 'g' 3) 'g' 4) 'g' 5) =
--      ((   0 'g' 4) 'g' 5) =
--      (    0 'g' 5) = 0   (where 'g' = const)
--
--  NOTE: Because of issues with spine traversal and non-strict 
--        evaluation, it is considered best practice to use foldl' 
--        instead of foldl. foldl' forces evaluation of values inside 
--        cons cells as it traverses the spine, instead of accumulating
--        unevaluated expressions as foldl does.
--
--    foldr const 0 [1..5] =
--      (1 'g' (2 'g' (3 'g' (4 'g' (5 'g' 0))))) =
--      (1 'g' (2 'g' (3 'g' (4 'g' 5)))) =
--      (1 'g' (2 'g' (3 'g' 4))) =
--      (1 'g' (2 'g' 3)) =
--      (1 'g' 2) = 1    (where 'g' = const)
--    



-- Exercise: Understanding folds
-- 1) b & c
-- 2)  foldl (*) 1 [1..5] = 
--      (((((1 `f` 1) 'f' 2) 'f' 3) 'f' 4) 'f' 5) =
--      (((( 1 'f' 2) 'f' 3) 'f' 4) 'f' 5) =
--      (((  2 'f' 3) 'f' 4) 'f' 5) =
--      ((   6 'f' 4) 'f' 5) =
--      (    24 'f' 5) = 120   (where `f` = flip (*))
-- 3) c
-- 4) a
-- 5) a) foldr (++) [] ["woot", "WOOT", "woot"]
--    b) foldr max ' ' "fear is the little death"
--    c) foldr (&&) True [False, True]
--    d) No. Because (||) will return True as long as one of the 
--       arguments is True.
--    e) foldl (flip ((++) . show)) "" [1..5] 
--    f) foldr (const . show) "a" [1..5]
--    g) foldr const '0' "tacos"
--    h) foldl (flip const) '0' "burritos"
--    i) foldl (flip (const . show)) "z" [1..5]



-- Exercise: Database Processing
--    refer to 10_dbProcessing.hs



-- Folding infinite lists
--
--    Because foldl has to traverse the spine completely before
--    evaluation, the following expression will never terminate.
--        
--        xs' = repeat 5 ++ [1,2,3]
--        rxs = reverse xs'
--        foldl (flip const) 0 rxs      -- never terminates
--
--    However, the foldr equivalent does
--        
--        xs = repeat 5 ++ [1,2,3]
--        foldr const 0 xs              -- evaluates to 5
--
--    This is because foldr disposes of the inner expressions once the
--    initial const expression containing the necessary information 
--    is constructed.



-- Breaking down the fibonacci no. generator example
--    
--    scanl :: (b -> a -> b) -> b -> [a] -> [b]
--    fibs = 1 : scanl (+) 1 fibs
--         = 1 : (1 : scanl (+) ((+) 1 1) fibs)
--         = 1 : (1 : (2 : scanl (+) ((+) 1 2) fibs))
--         = 1 : (1 : (2 : (3 : scanl (+) ((+) 2 3) fibs)))
--         ...
--    fibsN = fibs !! x     -- fibsN returns the xth element (0-indexed)



-- Exercise: Scans
fibs' :: Num a => [a]
fibs' = take 20 $ xs where xs = 1 : scanl (+) 1 xs

filteredFibs = takeWhile (< 100) $ xs where xs = 1 : scanl (+) 1 xs
-- using 'filter' causes the sequence to continue without termination

factorial :: Int -> Integer
factorial x = (!!) seq x where seq = scanl (*) 1 $ enumFrom 1 



-- Chapter Exercises
--    Warm up & review
--      1)
stops = "pbtdkg" :: String
vowels = "aeiou" :: String

--        a)
svs = [(x,y,z) | x <- stops, y <- vowels, z <- stops]
--        b)
svsPonly = [('p',y,z) | y <- vowels, z <- stops]

fst3 :: (a,a,a) -> a
fst3 (x,_,_) = x
svsPonly' = filter (\x -> (fst3 x) == 'p') svs
--        c)
-- I'm lazy to do this. The structure is the same


--      2)
-- seekritFunc takes a string, and finds the average letters per word 
-- within the string, rounded towards zero.

--      3)
seekritFunc' :: String -> Double 
seekritFunc' x = (/) (fromIntegral . sum . map length . words $ x)
                     (fromIntegral . length . words $ x)

-- Rewriting using folds
-- 1)
-- point free folding vers
myOr :: [Bool] -> Bool
myOr = foldr (||) False 
-- non point free folding vers
myOr' :: [Bool] -> Bool
myOr' = foldr (\x y -> if x then x else y) False

-- 2)
-- non point free folding vers
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x -> (||) . f $ x) False 

-- point free folding vers
-- TODO :: reference here -> https://bit.ly/3n0j94F
--         
--         I have a feeling it's not possible, 
--         but would like to try nonetheless

-- direct recursion
myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' f xs = go False f xs where
  go :: Bool -> (a -> Bool) -> [a] -> Bool
  go acc f [] = acc 
  go acc f (x:xs) = go (acc || (f x)) f xs

-- pattern guards
myAny''' :: Eq a => (a -> Bool) -> [a] -> Bool
myAny''' f (x:xs)
  | xs == []  = False  -- this guard necessitates type class constraint
  | f x = True || myAny''' f xs
  | otherwise = False || myAny''' f xs

-- 3)
-- folding 
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==) x) False

-- using 'any'
myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (== x) 


-- 4)
-- folding pointfree
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- direct recursion with ++
myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' (x:xs) = myReverse' xs ++ [x]

-- direct recursion without ++, but using an accumulator
myReverse'' :: [a] -> [a]
myReverse'' xs = go [] xs where
  go :: [a] -> [a] -> [a]
  go acc []     = acc
  go acc (x:xs) = go (x:acc) xs


-- 5)
-- foldr
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldr (\x -> (f x :)) []

-- 6)
-- foldr
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr ((++) . (\x -> if f x then [x] else [])) []

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f = foldr (\x acc -> if f x then x : acc else acc) []


-- 7) 
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []


-- 9) 
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> if (f x y == GT) then x else y) (last xs) xs 

-- 11)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x y -> if (f x y == LT) then x else y) (last xs) xs 
