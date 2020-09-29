module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime 
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- 1)
getTime :: DatabaseItem -> [UTCTime]
getTime (DbDate x) =[x]
getTime _ = []

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr ((++) . getTime) [] 

-- 2)
getNumber :: DatabaseItem -> [Integer]
getNumber (DbNumber x) = [x]
getNumber _ = []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr ((++) . getNumber) []

-- 3)
adEpoch :: UTCTime
adEpoch = UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 0)

mostRecent :: [DatabaseItem]
           -> UTCTime
mostRecent = foldr (max) adEpoch . filterDbDate 

-- 4)
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5)
avgDb :: [DatabaseItem] -> Double
avgDb xs =
  let dbLen = fromIntegral . length . filterDbNumber $ xs
      dbSum = fromInteger . sumDb $ xs
  in (/) dbSum dbLen
