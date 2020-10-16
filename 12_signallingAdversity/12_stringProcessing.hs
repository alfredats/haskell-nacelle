module StringProcessing where

import Data.Maybe


-- 1) Write a recursive function named replaceThe that takes
--    a text/string, breaks it into words, and replaces each instance
--    of "the" with "a". It should only replace exactly
--    the word "the".
notThe :: String -> Maybe String
notThe x = case x == "the" of
             True  -> Nothing
             False -> Just x

-- recursive
replaceThe :: String -> String
replaceThe = unwords . rf . words
  where
    rf :: [String] -> [String]
    rf [] = []
    rf (x:xs) = case (notThe x) == Nothing of 
                  True -> "a" : rf xs
                  False -> x : rf xs


-- non-recursive
replaceThe' :: String -> String
replaceThe' = unwords . map (rf . notThe) . words
  where 
    rf :: Maybe String -> String
    rf Nothing = "a"
    rf (Just x) = x

test_replaceThe :: IO ()
test_replaceThe = do
  if (==) (replaceThe "the cow") "a cow" 
     then putStrLn "replaceThe produces expected behavior w/ correct input"
     else putStrLn "replaceThe fails with correct input"
  if (==) (replaceThe "thecow") "thecow"
     then putStrLn "replaceThe produces expected behavior w/ incorrect input"
     else putStrLn "replaceThe fails with incorrect input"



-- 2) Write a recursive function that takes a text/string, breaks
--    it into words, and counts the number of instances of "the"
--    followed by a vowel-initial word
isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel 'A' = True
isVowel 'E' = True
isVowel 'I' = True
isVowel 'O' = True
isVowel 'U' = True
isVowel _   = False


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go 0 (Just "") . words
  where
    go :: Integer -> Maybe String -> [String] -> Integer
    go acc _ [] = acc
    go acc (Nothing) (x:xs) = 
      case isVowel . head $ x of 
        True  -> go (acc + 1) (notThe x) xs
        False -> go (acc)     (notThe x) xs
    go acc _  (x:xs) = go acc (notThe x) xs

test_countTheBeforeVowel :: IO ()
test_countTheBeforeVowel = do
  if (==) (countTheBeforeVowel "the evil cow") (1)
     then putStrLn "countTheBeforeVowel test 1 okay"
     else putStrLn "countTheBeforeVowel test 2 fail"
  if (==) (countTheBeforeVowel "a evil cow") (0)
     then putStrLn "countTheBeforeVowel test 2 okay"
     else putStrLn "countTheBeforeVowel test 2 fail"
  if (==) (countTheBeforeVowel "the the evil cow") (1)
     then putStrLn "countTheBeforeVowel test 3 okay"
     else putStrLn "countTheBeforeVowel test 3 fail"



-- 3) Return the number of letters that are vowels in a word
getVowels :: String -> String
getVowels [] = []
getVowels (x:xs) = case isVowel x of
                     True  -> x : getVowels xs
                     False -> getVowels xs

countVowels :: String -> Integer 
countVowels  = fromIntegral . length . getVowels
main :: IO ()
main = do 
  test_replaceThe
  test_countTheBeforeVowel 
