module EitherLibrary where 

import Data.Either

lefts' :: [Either a b] -> [a]
lefts' = foldr rf [] 
  where
    rf (Left x) [] = [x]
    rf (Right _) [] = []
    rf (Left x) xs = x : xs
    rf (Right _) xs = xs 

rights' :: [Either a b] -> [b]
rights' = foldr rf []
  where 
    rf :: Either a b -> [b] -> [b]
    rf (Right x) [] = [x]
    rf (Left  _) [] = []
    rf (Right x) xs = x:xs
    rf (Left  _) xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x


