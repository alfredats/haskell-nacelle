module Spec where

import Test.Hspec
import WordNumber (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "Zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "One"
  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100  `shouldBe` [1,0,0]
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "One-Zero-Zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "Nine-Zero-Zero-One"
