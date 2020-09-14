module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- 4) 
myRoundTrip :: (Show a, Read a) => a -> a
myRoundTrip = read . show

-- 5)
newRoundTrip :: (Show a, Read b) => a -> b
newRoundTrip a = read (show a)

main = do
  print (roundTrip 4)
  print (id 4)
  print (myRoundTrip 4)
  print ((newRoundTrip 4) :: Integer)
