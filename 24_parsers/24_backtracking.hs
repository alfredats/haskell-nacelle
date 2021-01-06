{-# LANGUAGE OverloadedStrings #-}
module BT where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)

import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

-- helper function to run a trifecta parser and print result
trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

-- helper func to run a parsec parser and print result
parsecP :: (Show a) => Parsec String () a -> String -> IO ()
parsecP = parseTest

-- helper func to run an attoparsec parser and print result
attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i


-- parsers
nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

tryAnnotated :: (Monad f, CharParsing f) => f Char
tryAnnotated = (try (char '1' >> char '2') <?> "Tried 12") <|>
               (char '3' <?> "Tried 3")

main :: IO ()
main = do 
  let x = "13"
  -- trif
  trifP nobackParse x
  trifP tryParse x

  let x = "13"
  -- parsec
  parsecP nobackParse x
  parsecP tryParse x

  let x = "13"
  -- attoparsec
  attoP nobackParse x
  attoP tryParse x



