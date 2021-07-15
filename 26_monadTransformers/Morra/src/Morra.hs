module Morra where

import System.IO
import Control.Monad (forever)
import System.Random
import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Text.Trifecta

---------------------------------------------------
--- Types
---------------------------------------------------
type MorraThrow = Integer
type MorraSum = Integer

data MorraIO = MorraInt Integer | MorraBound | MorraQuit deriving (Eq, Show)

type MorraScores = (Integer, Integer)
type Morra = StateT Game (ReaderT Config IO)

data Gametype = PvC 
newtype Config = Config { gametype :: Gametype }

data Game = Game { gameScore :: MorraScores
                 , pastThrows :: [MorraThrow]
                 }
---------------------------------------------------
--- Helpers 
---------------------------------------------------
morraThrowBounds :: (MorraThrow, MorraThrow)
morraThrowBounds = (0, 5)

morraSumBounds :: (MorraSum, MorraSum)
morraSumBounds = (0, 10)

randomInteger :: Integer -> Integer -> IO Integer
randomInteger x y = do 
    g <- getStdGen
    return . fst $ randomR (x, y) g

checkBounds :: (Integer, Integer) -> MorraIO -> Maybe Integer
checkBounds (f,l) (MorraInt x) 
  | (x >= f) && (x <= l) = Just x
  | otherwise = Nothing
checkBounds _ _ = Nothing

sumIsCorrect :: MorraSum -> MorraThrow -> MorraThrow -> Bool
sumIsCorrect sum x y = (x+y) == sum


---------------------------------------------------
--- IO & CPU 
---------------------------------------------------
cpuGuess :: IO (MorraThrow, MorraSum)
cpuGuess = do
  rInteger <- uncurry randomInteger morraThrowBounds
  rSum <- randomInteger rInteger $ snd morraSumBounds
  return (rInteger, rSum)

parseMorra :: Parser MorraIO
parseMorra = (MorraInt <$> integer) <|> (string "quit" >> return MorraQuit)

askPlayerGetReply :: String -> IO String
askPlayerGetReply i = putStr i >> getLine

handlePlayerInput :: String -> (Integer, Integer) -> String -> IO MorraIO
handlePlayerInput str bounds@(f,l) unparsed = do
  case parseString parseMorra mempty unparsed of
    Failure err -> putStrLn "Parse Error: Only integer characters or \"quit\" allowed" >> return MorraBound
    Success MorraQuit -> return MorraQuit
    Success parsed -> case checkBounds bounds parsed of
      Nothing -> putStrLn ("Bounding error: Input must be between " ++ show f ++ " & " ++ show l) >> return MorraBound
      Just x -> return $ MorraInt x


-- handleGuess :: Game -> MorraSum -> MorraThrow -> IO Game
-- handleGuess morra pSum pInteger = do
--     (cInteger, cSum) <- cpuGuess
--     putStrLn $ "The computer decided to throw" ++ show cInteger ++ ", and guessed the sum to be " ++ show cSum
--     putStrLn $ "You decided to throw " ++ show pInteger ++ ", and guessed the sum to be " ++ show pSum
--     putStrLn "What sum from 0-10 would you like to guess? \n"

--     case (sumIsCorrect cSum cInteger pInteger, sumIsCorrect pSum cInteger pInteger) of 
--         (True, True) -> do
--             putStrLn "A draw!"
--             putStrLn $ "Current Score is " ++ show cScore ++ ", " ++ show pScore
--         (True, False) -> do
--           putStrLn "Computer wins!"
--         (False, True) -> do
--           putStrLn "You win!"
--         (_, _) -> do
--           putStrLn "Nobody guessed correctly, moving on..."

        
