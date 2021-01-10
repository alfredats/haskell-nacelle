{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LogFileParser where

import Control.Applicative

import Text.Trifecta
import Text.RawString.QQ

import Test.Hspec
import qualified Test.QuickCheck as QC

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipWhitespace :: Parser ()
skipWhitespace = skipMany (oneOf "\t ")

commentEx :: String 
commentEx = "-- wheee a comment"

testComment = [r|
-- whee a comment ;

# header
|]

commentParse :: Parser () 
commentParse = 
  skipEOL >> char '-' >> char '-' >> skipMany alphaNum >> skipEOL

gotoEOL :: Parser ()
gotoEOL = try commentParse <|> skipEOL



type Year = Integer 
type Month = Integer
type Day = Integer
data Header = Header Year Month Day deriving (Eq, Ord, Show)

headerEx :: String 
headerEx = "# 2025-02-05"

headerEx' :: String 
headerEx' = "# 2025-02-07 -- dates not necessary"

headerParse :: Parser Header
headerParse = do
  _ <- char '#'
  skipWhitespace
  y <- some digit 
  _ <- char '-'
  m <- some digit
  _ <- char '-'
  d <- some digit
  skipOptional commentParse
  skipEOL
  return $ Header (read y) (read m) (read d)



type Hour = Integer
type Minute = Integer
data Time = Time Hour Minute deriving (Eq, Ord, Show)

type Activity = String

data Record = Record Time Activity deriving Show

recordEx :: String
recordEx = "09:00 Sanitizing moisture collector"

recordEx' :: String
recordEx' = "08:00 Breakfast -- should I try skippin bfast?"

timeParse :: Parser Time
timeParse = do
  h <- some digit
  _ <- char ':'
  m <- some digit
  return $ Time (read h) (read m)

activityParse :: Parser Activity
activityParse = do
  skipWhitespace
  token (some (noneOf "-"))

recordParse :: Parser Record
recordParse = do
  t <- timeParse
  a <- activityParse
  gotoEOL
  return $ Record t a








maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

test :: IO ()
test = hspec $ do
  let ps p i = parseString p mempty i
  describe "Log File parser" $ 
    it "parses a single comment, and skips to first day" $ do
      let m = ps (commentParse >> (char '#' <?> "parseHead") >> some letter) testComment
          r = maybeSuccess m
      r `shouldBe` Just "# header"
          
      
