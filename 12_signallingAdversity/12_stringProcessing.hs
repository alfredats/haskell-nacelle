module StringProcessing where

import Data.Maybe

notThe :: String -> Maybe String
notThe x = case x == "the" of
             True  -> Nothing
             False -> Just x


replaceThe :: String -> String
replaceThe = unwords . map (rf . notThe) . words
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

main :: IO ()
main = do 
  test_replaceThe
