module Morra where

import Control.Monad (forever)
import System.Random
import Control.Monad.Trans.State.Strict

---------------------------------------------------
--- Types
---------------------------------------------------
type MorraInt = Int 
type MorraSum = Int 

type MorraScores = (Int, Int)
type MorraGame = StateT MorraScores IO MorraInt



---------------------------------------------------
--- Helpers 
---------------------------------------------------
morraIntBounds :: (MorraInt,MorraInt)
morraIntBounds = (0, 5)

morraSumBounds :: (MorraSum, MorraSum)
morraSumBounds = (0, 10)

randomInt :: Int -> Int -> IO Int
randomInt x y = do 
    g <- getStdGen
    return . fst $ randomR (x, y) g

data Morra = Morra MorraInt [MorraInt] MorraInt

instance Eq Morra where
    (==) (Morra s1 gs1 g1) (Morra s2 gs2 g2) =
        (s1 == s2) && (gs1 == gs2) && (g1 == g2)


computerGuesses :: IO (MorraInt, MorraSum)
computerGuesses = do
  rInt <- uncurry randomInt morraIntBounds
  rSum <- randomInt rInt $ snd morraSumBounds
  return (rInt, rSum)


sumIsCorrect :: MorraSum -> MorraInt -> MorraInt -> Bool
sumIsCorrect sum x y = (x+y) == sum


handleGuess :: MorraGame -> MorraSum -> MorraInt -> IO MorraGame
handleGuess morra pSum pInt = do
    (cInt, cSum) <- computerGuesses 
    putStrLn $ "The computer decided to throw" ++ show cInt ++ ", and guessed the sum to be " ++ show cSum
    putStrLn $ "You decided to throw " ++ show pInt ++ ", and guessed the sum to be " ++ show pSum

    case (sumIsCorrect cSum cInt pInt, sumIsCorrect pSum cInt pInt) of 
        (True, True) -> do
            putStrLn "A draw!"
            putStrLn $ "Current Score is " ++ show cScore ++ ", " ++ show pScore
        (True, False) -> do
          putStrLn "Computer wins!"
        (False, True) -> do
          putStrLn "You win!"
        (_, _) -> do
          putStrLn "Nobody guessed correctly, moving on..."

        
