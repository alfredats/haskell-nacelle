{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M

import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans


-- goal : count hits to specific urls

data Config =
  Config {
    -- that's one, one click!
    -- two... two clicks!
    -- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }
  
type Scotty = 
  ScottyT Text (ReaderT Config IO)
type Handler = 
  ActionT Text (ReaderT Config IO)

-- what is the purpose of this function? 
--    this resembles an implementation of stateT
bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (M.insert k nc m, nc)
  where nc = fromMaybe 0 (M.lookup k m) + 1

app :: Scotty ()
app = get "/:key" $ do
    unprefixed <- param "key"
    config     <- lift $ ReaderT return
    let key' = mappend (prefix config) unprefixed     
    newInteger <- liftIO $ do
      cc <- readIORef (counts config)
      let (m', n) = bumpBoomp key' cc 
      writeIORef (counts config) m'
      return n

    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newInteger
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs -- list of prefixed keys obtained from cli
  -- initialize empty map
  counter <- newIORef M.empty -- counter :: IORef M.empty
  let config = Config { counts = counter, prefix = TL.pack prefixArg }
      runR r = runReaderT r config
  scottyT 3000 runR app
