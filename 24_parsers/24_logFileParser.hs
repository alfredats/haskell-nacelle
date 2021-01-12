{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LogFileParser where

import Control.Applicative
import Control.Monad (void)

import Data.Map as M
import Text.Trifecta
import Text.RawString.QQ

import Test.Hspec
import qualified Test.QuickCheck as QC



skipEOL :: Parser ()
skipEOL = skipMany newline

skipRestOfLine :: Parser ()
skipRestOfLine = 
  void (manyTill anyChar (void newline <|> eof))

skipCommentsAndSpaces :: Parser ()
skipCommentsAndSpaces = skipMany (commentParse <|> skipSome space)

commentEx :: String 
commentEx = "-- wheee a comment"

commentEx'' :: String
commentEx'' = "-- whee a comment\n \
\ -- whee another comment"

commentEx' :: String
commentEx' = [r|
-- whee a comment ;


# 2020-01-01
|]

commentParse :: Parser ()
commentParse = string "--" *> skipRestOfLine 



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

dateParse :: Parser Header
dateParse =
  Header <$> (integer <* char '-') <*> (integer <* char '-') 
    <*> integer

headerParse :: Parser Header
headerParse = char '#' *> space *> dateParse <* skipRestOfLine





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

recordEx''' :: String
recordEx''' = [r|
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym -- note the '-' in high-grav
12:00 Lunch
|]

timeParse :: Parser Time
timeParse = 
  Time <$> (read <$> some digit <* char ':') <*> (read <$> some digit)


activityParse :: Parser Activity
activityParse = 
  manyTill anyChar (commentParse <|> void newline <|> eof)
-- I'm not quite satisfied with the parsing output of 
-- activityParse with recordEx'. There is a trailing whitespace
-- character contained in the output. Perhaps this should be
-- accounted for within the comment parser?

recordParse :: Parser (Time, Activity) 
recordParse = 
  (,) <$> (spaces *> timeParse) <*> 
    (skipCommentsAndSpaces *> activityParse <* skipCommentsAndSpaces) 



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
14:00 Exercising in high-grav gym -- note the '-' in high-grav
|]

dayEx'' :: String
dayEx'' = [r|
-- this is a leading comment 

  
  -- this is another leading comment 
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
14:00 Exercising in high-grav gym -- note the '-' in high-grav
|]

data DayLog = DayLog Header Records deriving (Eq, Show)

--instance Show DayLog where
  --show (DayLog (Header y m d) workdone) = do
    --show $ "# " <> show y <> "-" <> show m <> "-" <> show d

dayLogParse :: Parser DayLog
dayLogParse = do
  skipCommentsAndSpaces
  h <- headerParse <?> "parse date"
  rs <- some recordParse <?> "parse records"
  return $ DayLog h (M.fromList rs)



logFileEx :: String 
logFileEx = [r|
-- wheee a comment

  -- another comment!
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


newtype LogFile = LogFile [DayLog] deriving (Eq, Show)

logFileParse :: Parser LogFile
logFileParse = do
  ds <- some (dayLogParse <* skipCommentsAndSpaces)
  return $ LogFile ds

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

test :: IO ()
test = hspec $ do
  let ps p i = parseString p mempty i
  describe "comment parsing" $ do
    it "parses a single comment, and skips to first day" $ do
      let p = skipEOL >> commentParse >> 
                void spaces >> skipEOL >> 
                  headerParse
          m = ps p commentEx'
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
      let m = parseString (recordParse <* eof) mempty recordEx
          r = maybeSuccess m
          expOut = 
            Just ( Time 9 0
                 , "Sanitizing moisture collector" :: Activity
                 ) 
      r `shouldBe` expOut
    
    it "record parser returns time & activity for single record \
    \with trailing comment" $ do
      let m = parseString recordParse mempty recordEx'
          r = maybeSuccess m
          expOut = 
            Just ( Time 8 0
                 , "Breakfast " -- note the trailing whitespace
                 )
      r `shouldBe` expOut

    it "record parser returns time & activity for single record \
    \with trailing comment, and containing single '-' in activity" $ do
      let m = parseString recordParse mempty recordEx''
          r = maybeSuccess m
          expOut = 
            Just ( Time 11 0
                 , "Exercising in high-grav gym " 
                 -- once again, the trailing whitespace
                 )
      r `shouldBe` expOut
  

  describe "dayLog parser" $ do
    let m x = parseString dayLogParse mempty x         
        expOut = 
          Just ( DayLog 
                  (Header 2025 02 07)
                  (fromList [ (Time 8 0, "Breakfast ")
                            , (Time 9 0, "Bumped head, passed out")
                            , (Time 13 36, "Wake up, headache")
                            , (Time 14 00, "Exercising in high-grav gym ")
                            ] )
                )
    
    it "parses a single day, and returns a daylog" $ do
      let r = maybeSuccess (m dayEx')
      r `shouldBe` expOut
    
    it "parses a single day (containing leading comments), and returns \
      \a day log" $ do
      let r = maybeSuccess (m dayEx'')
      r `shouldBe` expOut

  describe "log file parser" $ do
    it "parses a log file consisting of 2 days and returns appropriate\
\ logFile structure" $ do
      let m = parseString logFileParse mempty logFileEx
          r = maybeSuccess m
          expOut = 
            LogFile [ DayLog (Header 2025 2 5) 
                        (fromList [ (Time 9 0  , "Sanitizing moisture collector")
                                  , (Time 11 0 , "Exercising in high-grav gym")
                                  , (Time 12 0 , "Lunch")
                                  , (Time 13 0 , "Programming")
                                  , (Time 17 0 , "Commuting home in rover")
                                  , (Time 17 30, "R&R")
                                  , (Time 19 0 , "Dinner")
                                  , (Time 21 0 , "Shower")
                                  , (Time 21 15, "Read")
                                  , (Time 22 0,"Sleep")
                                  ]
                        )
                    , DayLog (Header 2025 2 7) 
                        (fromList [ (Time 8 0,"Breakfast ")
                                  , (Time 9 0,"Bumped head, passed out")
                                  , (Time 13 36,"Wake up, headache")
                                  , (Time 13 37,"Go to medbay")
                                  , (Time 13 40,"Patch self up")
                                  , (Time 13 45,"Commute home for rest")
                                  , (Time 14 15,"Read")
                                  , (Time 21 0,"Dinner")
                                  , (Time 21 15,"Read")
                                  , (Time 22 0,"Sleep")
                                  ]
                        )
                    ]
      r `shouldBe` Just expOut
