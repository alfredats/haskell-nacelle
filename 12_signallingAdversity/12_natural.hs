module Natural where

-- Your task will be to
-- implement functions to convert natural numbers to integers
-- and integers to naturals. The conversion from Nat to Integer
-- won’t return Maybe, because—as you know—Integer is a strict
-- superset of Nat. Any Nat can be represented by an Integer, but
-- the same is not true of any Integer.

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

test_natToInt :: IO ()
test_natToInt = do
  if (natToInteger Zero) == 0
     then putStrLn "natToInteger test 1 ok"
     else putStrLn "natToInteger test 1 no go"
  if (natToInteger (Succ (Succ Zero))) == 2
     then putStrLn "natToInteger test 2 ok"
     else putStrLn "natToInteger test 2 no go"

integerToNat :: Integer -> Maybe Nat
integerToNat int =
  let go 0 = Zero
      go x = Succ (go (x-1))
   in case int >= 0 of
        True  -> Just (go int)
        False -> Nothing

test_intToNat :: IO ()
test_intToNat = do
  if (integerToNat 0) == Just Zero
     then putStrLn "integerToNat test 1 okay"
     else putStrLn "integerToNat test 1 no go"
  if (integerToNat 2) == Just (Succ (Succ Zero))
     then putStrLn "integerToNat test 2 okay"
     else putStrLn "integerToNat test 2 no go"
  if (integerToNat (-1)) == Nothing
     then putStrLn "integerToNat test 3 okay"
     else putStrLn "integerToNat test 3 no go"

main :: IO ()
main = do 
  test_natToInt
  test_intToNat
