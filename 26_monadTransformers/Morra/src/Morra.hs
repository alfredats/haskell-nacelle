module Morra where

import System.Random 
import System.IO ()
import System.Console.ANSI (clearScreen)
import Control.Applicative ( Alternative((<|>)) )
import Control.Monad (forever)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.State.Strict ( StateT(..) )
import Control.Monad.Trans.Reader ( ReaderT(..) )
import Text.Trifecta
    ( parseString,
      integer,
      Result(Success, Failure),
      CharParsing(string),
      Parser )

---------------------------------------------------
--- Types
---------------------------------------------------
type MorraThrow = Integer
type MorraSum = Integer

data MorraIO = MorraInt Integer | MorraBound | MorraQuit deriving (Eq, Show)

type MorraScores = (Integer, Integer)
type Morra = StateT Game (ReaderT Config IO)

data Gametype = PvC deriving Eq
newtype Config = Config { gametype :: Gametype }

data Game = Game { gameScore :: MorraScores
                 , pastThrows :: [MorraThrow]
                 , config :: Config
                 }
---------------------------------------------------
--- Helpers 
---------------------------------------------------
morraThrowBounds :: (MorraThrow, MorraThrow)
morraThrowBounds = (0, 5)

morraSumBounds :: (MorraSum, MorraSum)
morraSumBounds = (0, 10)

randomInteger :: Integer -> Integer -> IO Integer 
randomInteger x y = randomRIO (x, y) 

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

handlePlayerInput :: (Integer, Integer) -> String -> IO MorraIO
handlePlayerInput bounds@(f,l) unparsed = do
  case parseString parseMorra mempty unparsed of
    Failure err -> putStrLn "Parse Error: Only integer characters or \"quit\" allowed" >> return MorraQuit
    Success MorraQuit -> return MorraQuit
    Success parsed -> case checkBounds bounds parsed of
      Nothing -> putStrLn ("Bounding error: Input must be between " ++ show f ++ " & " ++ show l) >> return MorraBound
      Just x -> return $ MorraInt x

playerIO :: String -> (Integer,Integer) -> IO MorraIO
playerIO str bounds = do
  unp <- askPlayerGetReply str
  x <- handlePlayerInput bounds unp
  case x of
    (MorraInt i) -> pure x 
    _ -> playerIO str bounds



---------------------------------------------------
--- Morra Game helpers
---------------------------------------------------

updateScores :: (MorraThrow, MorraSum) -> (MorraThrow, MorraSum) -> Morra ()
updateScores (t1, s1) (t2, s2) = StateT $ \g-> ReaderT $ \c -> do
  let (x, y) = gameScore g
      pt = (:) t2 $ pastThrows g 
      conf = config g
  liftIO $ putStrLn $ "You threw " ++ show t1 ++ " & guessed the sum would be " ++ show s1
  liftIO $ putStrLn $ "CPU threw " ++ show t2 ++ " & guessed the sum would be " ++ show s2
  case (sumIsCorrect s1 t1 t2, sumIsCorrect s2 t1 t2) of 
      (False, True) -> do
        liftIO $ putStrLn "CPU wins!"
        return ((), Game { gameScore = (x, y + 1), pastThrows = pt, config = conf})
      (True, False) -> do
        liftIO $ putStrLn "You win!"
        return ((), Game { gameScore = (x + 1, y), pastThrows = pt, config = conf})
      (_,_) -> do
        liftIO $ putStrLn "A draw!"
        return ((), Game { gameScore = (x, y), pastThrows = pt, config = conf})

printScores :: Game -> IO () 
printScores g = do
  let (x, y) = gameScore g
  putStrLn "Current score:"
  putStrLn $ "  Player: " ++ show x
  putStrLn $ "  Computer: " ++ show y
  putStrLn ""

morraGame :: Morra ()
morraGame =  do
  (MorraInt pT) <- liftIO $ playerIO "What would number like to throw? " morraThrowBounds
  (MorraInt pS) <- liftIO $ playerIO "What do you guess the sum will be? " morraSumBounds
  cpuG <- liftIO cpuGuess
  updateScores (pT, pS) cpuG        

runGame :: IO ()
runGame = clearScreen >>= \s -> forever $ do
  putStrLn "####################"
  putStrLn "morra! morra! morra!"
  putStrLn "####################"
  (_, g) <- flip runReaderT (Config PvC) $ runStateT morraGame (Game (0,0) [] $ Config PvC)
  printScores g


