module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = 
    frequency [ (1, return Fools)
              , (1, return Twoo)]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where
  (=-=) = eq

type SSI = (String, String, Int)

main :: IO ()
main = do
  quickBatch (monoid Twoo)
  quickBatch $ applicative trigger
    where 
      trigger :: [SSI]
      trigger = undefined 




