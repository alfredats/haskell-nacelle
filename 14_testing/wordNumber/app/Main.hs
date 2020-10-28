module Main where

import WordNumber (wordNumber)
import System.IO

main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering
  putStr "Enter an integer: "
  num <- getLine
  putStrLn $ wordNumber (read num)
