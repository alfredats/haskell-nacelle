module Standard where

-- example 
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs


myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = (a == x) || myElem a xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = go xs []
  where 
    go []     memo    = memo
    go (y:ys) memo = go ys (y:memo)

squish :: [[a]] -> [a]
squish [] = []
squish (xs:xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []  = error "Empty List"
myMaximumBy f lst = go f lst (head lst)
  where 
    go f [] currmax = currmax
    go f (x:xs) currmax 
      | f x  currmax == GT = go f xs x
      | otherwise          = go f xs currmax


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []  = error "Empty List"
myMinimumBy f lst = go f lst (head lst)
  where 
    go f [] currmin = currmin
    go f (x:xs) currmin 
      | f x  currmin == LT = go f xs x
      | otherwise          = go f xs currmin

myMax :: (Ord a) => [a] -> a
myMax = myMaximumBy (compare)

myMin :: (Ord a) => [a] -> a
myMin = myMinimumBy (compare)

