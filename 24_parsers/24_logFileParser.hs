{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LogFileParser where

import Control.Applicative

import Data.Map as M
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

commentEx' :: String
commentEx' = [r|
-- whee a comment ;

# 2020-01-01 
|]

commentParse :: Parser () 
commentParse = 
    skipEOL >> 
      string "--" >> 
        skipMany (noneOf "\n") >> 
          skipEOL

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

headerEx'' :: String
headerEx'' = [r|
# 2020-02-08 -- dates not necessarily sequential
|]

headerParse :: Parser Header
headerParse = do
  skipEOL
  _ <- char '#'
  skipWhitespace
  y <- some digit 
  _ <- char '-'
  m <- some digit
  _ <- char '-'
  d <- some digit
  gotoEOL
  return $ Header (read y) (read m) (read d)




type Hour = Integer
type Minute = Integer
data Time = Time Hour Minute deriving (Eq, Ord, Show)

type Activity = String
type Records = Map Time Activity 

recordEx :: String
recordEx = "09:00 Sanitizing moisture collector"

recordEx' :: String
recordEx' = "08:00 Breakfast -- should I try skippin bfast?"

recordEx'' :: String
recordEx'' =
  "11:00 Exercising in high-grav gym -- note the '-' in high-grav"
timeParse :: Parser Time
timeParse = do
  h <- some digit
  _ <- char ':'
  m <- some digit
  skipWhitespace
  return $ Time (read h) (read m)

activityParse :: Parser Activity
activityParse = do
  skipWhitespace
  sepEndBy anyChar (commentParse <|> skipEOL)
-- I'm not quite satisfied with the parsing output of 
-- activityParse with recordEx'. There is a trailing whitespace
-- character contained in the output. Perhaps this should be
-- accounted for within the comment parser?

recordParse :: Parser (Time, Activity) 
recordParse = do
  t <- timeParse
  a <- activityParse
  return $ (,) t a



dayEx :: String
dayEx = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
|]

dayEx' :: String
dayEx' = [r|
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
|]

dayEx'' :: String
dayEx'' = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym -- note the '-' in high-grav
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
|]

data DayLog = DayLog Header Records deriving (Eq, Show)

getRecords :: DayLog -> Records
getRecords (DayLog _ recs) = recs

--instance Show DayLog where
  --show (DayLog (Header y m d) workdone) = do
    --show $ "# " <> show y <> "-" <> show m <> "-" <> show d

dayLogParse :: Parser DayLog
dayLogParse = do
  skipEOL
  h <- token headerParse <?> "parse date"
  rs <- many recordParse <?> "parse records"
  return $ DayLog h (M.fromList rs)



logFileEx :: String 
logFileEx = [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]


newtype LogFile = LogFile [DayLog] deriving Show

logFileParse :: Parser LogFile
logFileParse = do
  skipEOL
  commentParse
  ds <- some dayLogParse
  gotoEOL
  return $ LogFile ds

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

test :: IO ()
test = hspec $ do
  let ps p i = parseString p mempty i
  describe "comment parsing" $ do
    it "parses a single comment, and skips to first day" $ do
      let m = ps (commentParse >> headerParse) commentEx'
          r = maybeSuccess m
      r `shouldBe` Just (Header 2020 01 01)
  

  describe "date header parsing" $ do
    it "parses a single date" $ do
      let m = ps headerParse headerEx
          r = maybeSuccess m
      r `shouldBe` Just (Header 2025 02 05)

    it "parses a single date (with trailing comment)" $ do
      let m = ps headerParse headerEx'
          r = maybeSuccess m
      r `shouldBe` Just (Header 2025 02 07)
  

  describe "record parser" $ do  
    it "time parser returns time for single record" $ do
      let m = parseString timeParse mempty recordEx
          r = maybeSuccess m
      r `shouldBe` Just (Time 9 0)
    
    it "record parser returns time & activity for single record" $ do
      let m = parseString recordParse mempty recordEx
          r = maybeSuccess m
          expOut = 
            Just ( Time 9 0
                 , "Sanitizing moisture collector" :: Activity
                 ) 
      r `shouldBe` expOut
    
    it "record parser returns time & activity for single record with trailing comment" $ do
      let m = parseString recordParse mempty recordEx'
          r = maybeSuccess m
          expOut = 
            Just ( Time 8 0
                 , "Breakfast " -- note the trailing whitespace
                 )
      r `shouldBe` expOut
    it "record parser returns time & activity for single record with trailing comment, and containing single '-' in activity" $ do
      let m = parseString recordParse mempty recordEx''
          r = maybeSuccess m
          expOut = 
            Just ( Time 11 0
                 , "Exercising in high-grav gym " -- once again, the trailing whitespace
                 )
      r `shouldBe` expOut
  describe "dayLog parser" $ do
    it "parses a single day, and returns a daylog" $ do
      let m = parseString dayLogParse mempty dayEx'
          r = maybeSuccess m
          expOut = 
            Just ( DayLog 
                  (Header 2025 02 05)
                  (fromList [ (Time 8 0, "Breakfast")
                            , (Time 9 0, "Sanitizing moisture collector")
                            ] )
                )
      r `shouldBe` expOut
