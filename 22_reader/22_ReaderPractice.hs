module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

-- The next thing we want to do is write some functions that
-- zip those lists together and use lookup to find the value associ-
-- ated with a specified key in our zipped lists.

-- lookup :: Eq a => a -> [(a,b)] -> Maybe b

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer 
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)


-- Next, we’re going to make some helper functions. Let’s use
-- uncurry to allow us to add the two values that are inside a tuple

-- uncurry :: (a -> b -> c) -> (a, b) -> c

summed :: Num c => (c, c) -> c
summed = uncurry (+) 

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

-- using fromMaybe :: a -> Maybe a -> a
sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m


main :: IO ()
main = do 
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ foldr (&&) True (sequA 7)
  print $ sequA (fromMaybe 0 s')
  print $ bolt (fromMaybe 0 ys)
    where s' = summed <$> ((,) <$> xs <*> ys)
