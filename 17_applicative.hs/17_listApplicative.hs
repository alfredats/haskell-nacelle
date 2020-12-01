module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Datatype definition

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where 
  (<>) Nil x = x
  (<>) x Nil = x
  (<>) (Cons a b) xy@(Cons _ _) = Cons a (b <> xy)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)  

instance Applicative List where
  pure x = Cons x Nil 
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f g) xy@(Cons _ _) = (f <$> xy) <> (g <*> xy)


listGen :: Arbitrary a => Gen (List a)
listGen = frequency [ (1, return Nil)
                    , (1, Cons <$> arbitrary <*> listGen)]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

instance Eq a => EqProp (List a) where
  (=-=) = eq

-----------------------------------------------------------------------


append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ f <$> as

type ListSemi = (List String, Integer)
type ListSSI = List (String, String, Integer)

main :: IO ()
main = do
  quickBatch $ semigroup (trigger :: ListSemi)
  quickBatch $ functor (trigger :: ListSSI)
  quickBatch $ applicative (trigger :: ListSSI)
    where trigger = undefined
