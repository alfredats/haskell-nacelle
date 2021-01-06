{-# LANGUAGE OverloadedStrings #-}
module Chapter24 where

import Text.Trifecta
import Text.Parser.Combinators
import Data.Ratio

import Control.Applicative
import Control.Monad.Trans.State

-- Parser Combinators
--
--  In this chapter we aren't writing parsers per se, but using 
--  the reader and state analogues we've studied in the past 
--  2 chapters to learn parser combinators.
--
--    type Parser a = String -> Maybe (a, String)
--
--  The Parser datatype takes a string value, and produces a 
--  result that may or may not have succeeded, before returning
--  a tuple of the value wanted and whatever's left of the string
--  that wasn't consumed to produce a value of type a.
--
--  The idea with the Parser type is that State will handle the
--  fact an eventual text input, and having parsed something out
--  would have altered the input and resulted in a new state of 
--  the input stream. It also allows return of a value independent
--  of the state, while Maybe handles the possibility of parser 
--  failure.
--
--
--
-- An example
stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

parserEg = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo':"
  testParse oneTwo'


-- Exercises : Parsing Practice
--
-- 1. Use the eof function to create a parser combinator that
--    fails, due to the fact that it does not finish at the 
--    end-of-file. (I made one fail and oneTwo succeed instead)
oneEOF = one >> eof
oneTwoEOF = oneTwo >> eof

testParse' :: Parser () -> IO ()
testParse' p =
  print $ parseString p mempty "12"

-- 2. Use string to make a parser that parses "1", "12" and "123"
--    out of the example input, respectively.
p123 :: String -> IO ()
p123 str = 
  print $ parseString (string str) mempty str

-- 3. Try writing a Parser that does what string does, but 
--    using char
stringParse :: CharParsing m => String -> m String
stringParse = traverse char 

p123' :: String -> IO ()
p123' str =
  print $ parseString (stringParse str) mempty str


--
--
--
--  Something to note is that with Text.Trifecta (and other 
--  parsers in general), it is not a given that a single 
--  parser exhausts all input. It can be such that they only
--  consume as much text as they need to produce the value of 
--  the type requested.
--
notAllConsumed = do
  print $ parseString (char 'a') mempty "abcdef"
  print $ parseString (string "abc") mempty "abcdef"


-- Parsing fractions
--  
--  This one requires the language pragma OverloadedStrings. We'll
--  see why in a bit. 
--
badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of 
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)


-- decimal :: TokenParsing m => m Integer
-- decimal is a parsing function like char or string, specialized
-- for decimal numbers

pF :: IO ()
pF = do 
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

vF :: IO ()
vF = do
  let virtuousFraction' = parseString virtuousFraction mempty
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad


-- Exercise : Unit of Success
unitSuccess :: Parser Integer
unitSuccess = do
  ints <- integer
  eof
  return ints

intOnly :: IO ()
intOnly = do 
  print $ parseString unitSuccess mempty "123abc"
  print $ parseString unitSuccess mempty "123"
  print $ parseString unitSuccess mempty "12"


-- The Alternative datatype
--  refer to 24_altParsing.hs
--
--
-- Polymorphic Parsers
--  refer to 24_polymorphicParsers.hs
--
--
-- Backtracking
--  refer to 24_backtracking.hs
--
--
-- Marshalling & Unmarshalling
--  refer to 24_marshallingJSON.hs
--
--
--
--  Chapter Exercises
--
-- 1. Write a parser for semantic versions as defined by 
--    http://semver.org/. Also, write an Ord instance for the
--    SemVer type that obeys the specification outlined on the
--    SemVer website.
--    
--      refer to 24_semver.hs
--
