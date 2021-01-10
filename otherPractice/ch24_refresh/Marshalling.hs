{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Marshalling where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ

sectionJSON :: ByteString
sectionJSON = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]


-- 
data TestData =
  TestData {
    section :: Host 
  , what :: Color
           } deriving (Eq, Show)

newtype Host = 
  Host String deriving (Eq, Show)

type Annotation = String

data Color =
    Red Annotation
  | Blue Annotation
  | Yellow Annotation
  deriving (Eq, Show)

main = do
  let d :: Maybe TestData
      d = decode sectionJson
  print d
