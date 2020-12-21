module WarmingUp where

import Data.Char
import Test.Hspec

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

composedIsFmapped :: IO ()
composedIsFmapped = hspec $ do
  describe "composed is fmapped" $ do
    it "composed  \"hello\" == fmapped \"hello\"" $ do
      composed "hello" `shouldBe` fmapped "hello"


tupled :: [Char] -> ([Char], [Char]) 
tupled = (,) <$> id <*> composed

tupled' :: [Char] -> ([Char], [Char])
tupled' = id >>= 
  \f -> composed >>= 
    \g -> return (f,g) 
