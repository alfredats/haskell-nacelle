module LearnParsers where

import Text.Trifecta
import Data.Ratio ((%))
import Control.Applicative

stop :: Parser a 
stop = unexpected "stop"

-- read a single character '1'
one = char '1'

-- read a single character '1', then die
one' = one >> stop

-- recall that (>>) :: Monad m => m a -> m b -> m b,
-- thus any output from one in one' is discarded. However, the 
-- effect of 'moving the cursor' is still retained. So it is like 
-- having State.
--
-- 
--    newtype State  s a = State { runState :: s -> (a, s) }
--    newtype Parser   a = String -> Maybe (a, String)
--
--  State is just a wrapper for a function that takes a 'state' input
--  and returns a tuple of expected output (if any) together with the
--  new 'state'. Parser is simply State specialized to string 
--  processing.
--
--  Parser allows us to return a value independent of the state, while
--  allowing Maybe to handle the possibility of parser failure. Perhaps
--  a deeper look at the 'char' function used in 'one' would be helpful
--  in illustrating that idea:
--
--    (not actual implementation, just for illustration)
--    char :: Char -> Parser Char
--    char c = 
--      Parser $ \s -> 
--        case s of 
--          (x:xs) -> if c == x then [(c, xs)] else []
--          _ -> []
--    


-- read two characters, '1' and '2'
oneTwo = char '1' >> char '2'

-- read '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"


pNL s = putStrLn ('\n' : s)
-- exercise : parsing practice
oneEOF :: Parser ()
oneEOF = char '1' >> eof

oneTwoEOF :: Parser () 
oneTwoEOF = char '1' >> char '2' >> eof

testParse' :: Parser () -> IO ()
testParse' p = 
  print $ parseString p mempty "123"

p123 :: String -> IO ()
p123 str =
  print $ parseString (string str) mempty str

myString :: String -> Parser String
myString = traverse char

parsingPractice :: IO ()
parsingPractice = do
  pNL "oneEOF"
  testParse' oneEOF

  pNL "oneTwoEOF"
  testParse' oneTwoEOF

  pNL "p123"
  p123 "1"
  p123 "12"
  p123 "123"

  pNL "myString"
  (\x -> print $ parseString (myString x) mempty x) "123"


-- Exercise : Unit Of Success
intEOF :: Parser Integer
intEOF = do
  int <- integer
  eof
  return int


-- Exercise : Try Try
--  make a parser that can parse either decimals or fractions. You'll
--  want to use <|> from Alternative.
frac :: Parser Rational
frac = do 
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of 
    0 -> fail "denominator cannot be zero"
    _ -> return (numerator % denominator)


type FracOrInt = Either Rational Integer 

parseFracOrInt :: Parser FracOrInt 
parseFracOrInt = 
      (Left <$> try frac)
  <|> (Right <$> decimal)
