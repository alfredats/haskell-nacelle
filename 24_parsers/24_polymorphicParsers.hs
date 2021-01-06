{-# LANGUAGE OverloadedStrings #-}
module Text.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (MonadFail m, TokenParsing m) => m Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  
  case denominator of 
    0 -> fail "Denominator must be non-zero"
    _ -> return (numerator % denominator)


main :: IO ()
main = do
  -- parseOnly is Attoparsec
  let attoP = parseOnly parseFraction
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork
  print $ attoP badFraction
  print $ attoP alsoBad
  
  -- parseString is Trifecta
  let p f i = parseString f mempty i
  print $ p parseFraction shouldWork
  print $ p parseFraction shouldAlsoWork
  print $ p parseFraction badFraction
  print $ p parseFraction alsoBad

