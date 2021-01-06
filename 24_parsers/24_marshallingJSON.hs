{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Marshalling where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ


sectionJSON :: ByteString
sectionJSON = [r|
{ "section" : {"host": "wikipedia.org"},
  "whatisit": {"red" : "intoothandclaw"}
}
|]


--  The common functions for using aeson are the following
--
--    encode :: ToJSON a => a -> Data.ByteString.Lazy.ByteString
--    decode :: FromJSON a => Data.ByteString.Lazy.ByteString -> Maybe a
--

demoDecode = decode sectionJSON :: Maybe Value
--  Unless given a type, the type defaulting in Haskell will give 
--  the output of decode a nonsense value.


newtype Host = Host String deriving (Eq, Show)

type Annotation = String

data Color = 
    Red Annotation
  | Blue Annotation
  | Yellow Annotation
  deriving (Eq, Show)


data TestData = 
  TestData {
    section :: Host
  , what :: Color
           } deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) = 
    TestData <$> v .: "section"
             <*> v .: "whatisit"
  parseJSON _ = 
    fail "Expected an object for TestData"

instance FromJSON Host where
  parseJSON (Object v) = 
    Host <$> v .: "host"
  parseJSON _ = 
    fail "Expected an object for Host"

instance FromJSON Color where
  parseJSON (Object v) =
        (Red <$> v .: "red")
    <|> (Blue <$> v .: "blue")
    <|> (Yellow <$> v .: "yellow")
  parseJSON _ =
    fail "Expected an object for Color"

main = do 
  let d :: Maybe TestData
      d = decode sectionJSON
  print d
