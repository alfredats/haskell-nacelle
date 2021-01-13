module IPv4parser where

import Control.Applicative 
import Control.Monad (void)

import Data.Word
import Text.Trifecta

import Test.Hspec

-- data flow:
--  1. given an ipv4 address, break it down into individual octets
--  2. parse each octet, and convert from word8 to binary string
--  3. concatenate binary strings from each octet, and convert to word32

data IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

parseOctet :: Parser Word8
parseOctet = read <$> some octDigit 

w8ToBinStr :: Word8 -> String
w8ToBinStr x = go x []
                where 
                  go y acc
                        | y == 0 = 
                          (<>) (replicate (8 - length acc) '0') acc  
                        | mod y 2 == 1 = go (div y 2) ('1': acc)
                        | mod y 2 == 0 = go (div y 2) ('0': acc)

binStrToW32 :: String -> Word32
binStrToW32 x = go x 31 0 
                  where
                    go :: String -> Word32 -> Word32 -> Word32
                    go [] _ acc = acc
                    go (x:xs) y acc 
                      | x == '1' = go xs (y - 1) (acc + 2 ^ y)
                      | x == '0' = go xs (y - 1) acc


parseIPv4 :: Parser IPAddress 
parseIPv4 = do 
  as <- some (parseOctet <* (void (char '.') <|> void newline <|> eof)) 
  return $ IPAddress (binStrToW32 (foldr ((<>) . w8ToBinStr) mempty as))

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

test :: IO ()
test = hspec $ do
  let ps p s = parseString p mempty s
  describe "octet parser" $ do 
    it "parses a string and returns appropriated word8 data" $ do
      let m = ps parseOctet "172"
          r = maybeSuccess m
      r `shouldBe` Just (172 :: Word8)
  describe "w8ToBinStr" $ do
    it "given word8 data, converts to appropriate\ 
      \ 8bit binary string" $ do
      w8ToBinStr (10 :: Word8) `shouldBe` "00001010"
  describe "IPv4 parser" $ do
    it "given an IPv4 address, returns the appropriate Word32\
       \ data" $ do
        let m = ps parseIPv4 "172.16.254.1"
            r = maybeSuccess m
        r `shouldBe` Just (IPAddress 2886794753)
        


