{-# LANGUAGE QuasiQuotes #-}
module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta


type NumberOrString = Either Integer String 


eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

-- The '[r|' denotes a quasi-quoted section using the quasi-quoter 
-- named 'r'. Note that the QuasiQuotes pragma has to be enabled for it 
-- to work.


parseNos :: Parser NumberOrString
parseNos = 
      (Left <$> integer)
  <|> (Right <$> some letter)
-- this one doesn't work because it doesn't account for the newline
-- characters within the quasiquoted string

parseNos' :: Parser NumberOrString
parseNos' =
  skipMany (oneOf "\n") -- this allows us to "skip" a newline character
  >>
      (Left <$> integer)
  <|> (Right <$> some letter)
  -- However, it expects to match an integer or letters with the final 
  -- newline after def in eitherOr but runs into eof, and throws an err

parseNos'' :: Parser NumberOrString
parseNos'' = do
  skipMany (oneOf "\n")
  v <-      (Left <$> integer)
        <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v
-- This one accounts for possible newlines before and after integers or
-- letters, and thus works.
--
-- alternatively, we can tokenize parseNos

main = do 
  let p f i = parseString f mempty i
  print $ p parseNos                 eitherOr
  print $ p parseNos'                eitherOr
  print $ p (some parseNos')         eitherOr
  print $ p (some parseNos'')        eitherOr
  print $ p (some (token parseNos')) eitherOr
