module ValidateWord where

import Data.Maybe

-- Use the Maybe type to write a function that counts the number of 
-- vowels in a string, and the number of consonants. If the number
-- of vowels exceeds the number of consonants, the function returns 
-- Nothing.

-- In many human languages, vowels rarely exceed the number of 
-- consonants, so when they do, it may indicate that the input is not 
-- a valid word


newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

countVowels = go (0,0)
  where 
    go :: (Integer, Integer) -> String -> (Integer, Integer)
    go acc [] = acc
    go (vow,con) (x:xs) = case elem x vowels of 
                            True  -> go (vow+1,con) xs
                            False -> go (vow,con+1) xs

mkWord :: String -> Maybe Word'
mkWord str = case (fst vc) <= (snd vc) of 
               True  -> Just (Word' str)
               False -> Nothing
             where vc = countVowels str

test_mkWord :: IO ()
test_mkWord = do
  if (mkWord "hello") == Just (Word' "hello") 
     then putStrLn "mkWord test 1 okay"
     else putStrLn "mkWord test 1 not okay"
  if (mkWord "abediou") == Nothing
     then putStrLn "mkWord test 2 okay"
     else putStrLn "mkWord test 2 not okay"
