module Palindrome where

import System.Exit (exitSuccess)
import Control.Monad (forever)
import Data.Char

-- (2)
palindrome :: IO ()
palindrome = forever $ do 
  line1 <- getLine
  case (line1 == reverse line1) of 
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

-- (3)
strToLower :: String -> String
strToLower = map toLower

palindrome' :: IO ()
palindrome' = forever $ do 
  line1 <- getLine
  let proLine = filter isAlpha . strToLower $ line1
   in case (proLine == reverse proLine) of 
        True -> putStrLn "It's a palindrome!"
        False -> do
          putStrLn "Nope!"
          exitSuccess
