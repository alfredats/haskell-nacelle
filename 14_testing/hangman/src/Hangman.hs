module Hangman where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)
import System.Random (randomRIO)



-- type WordList = [String]
newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int 
minWordLength = 5

maxWordLength :: Int
-- maxWordLength = 9
maxWordLength = 7 -- modified for easier games

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
    where gameLength w = 
            let l = length (w :: String)
            in     l >= minWordLength
               &&  l <  maxWordLength 

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl - 1))
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

--


data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = 
    (intersperse ' ' $
      fmap renderPuzzleChar discovered)
      ++ " Guessed so far: " ++ guessed ++ "\n"

instance Eq Puzzle where
  (==) (Puzzle w1 d1 g1) (Puzzle w2 d2 g2) =
    w1 == w2 && d1 == d2 && g1 == g2

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle xs [const Nothing x | x <- xs] []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c:s)
    where 
      zipper guessed wordChar guessChar =
        if wordChar == guessed
           then Just wordChar
           else guessChar 
      newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do

  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of

    (_, True) -> do
      putStrLn "You already guessed that character, pick\
               \ something else!"
      return puzzle

    (True, _) -> do 
      putStrLn "This character was in the word, filling \
               \ in the word accordingly"
      return (fillInCharacter puzzle guess)

    (False, _) -> do
      putStrLn "This character wasn't in the word, try again!"
      return (fillInCharacter puzzle guess)


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess discovered guessed) =
  -- if (length guessed) > 7
  if (lg - lc) > 10 -- increased number of chances to guess & 
     then do        -- counts only incorrect guesses
       putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
      else return ()
  where 
    lc = length $ filter isJust discovered
    lg = length guessed

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = 
  if all isJust filledInSoFar 
     then do
       putStrLn "You win!"
       exitSuccess
     else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"


