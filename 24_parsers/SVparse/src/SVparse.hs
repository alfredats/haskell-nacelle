module SVparse where

import Text.Trifecta
import Control.Applicative

data NumberOrString = NOSS String
                    | NOSI Integer
                    deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata 
  deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer x y z _ _) (SemVer a b c _ _) =
    case compare x a of 
      GT -> GT
      LT -> LT
      _  -> case compare y b of 
              GT -> GT
              LT -> LT
              _  -> case compare z c of 
                      GT -> GT
                      EQ -> EQ
                      _  -> LT

type MMP = (Major, Minor, Patch) 
type RelMet = Either () String

parserMMP :: Parser MMP
parserMMP = do
  vMaj <- integer
  _ <- char '.'
  vMin <- integer
  _ <- char '.'
  vPatch <- integer
  return (vMaj, vMin, vPatch) 

parseNumberOrString :: (Monad m, TokenParsing m) => 
  NumberOrString -> m NumberOrString
parseNumberOrString (NOSS str) = return $ NOSS str
parseNumberOrString (NOSI int) = return $ NOSI int

noDots :: Parser [NumberOrString]
noDots = some $ do
  i <- token (some digit)
  char '.'
  return (read i)

parseRel :: Parser [NumberOrString]
parseRel = undefined


parseSemVer :: Parser SemVer
parseSemVer = do
  (major, minor, patch) <- parserMMP
  isEnd <- Left <$> eof <|> Right <$> char '-'
  case isEnd of 
    Right '-' -> do
      vRel <- parseRel
      return $ SemVer major minor patch vRel [] 
    _   -> return $ SemVer major minor patch [] []
  
pMMP :: String -> IO ()
pMMP str = print $ parseString parserMMP mempty str


psv :: String -> IO ()
psv str = print $ parseString parseSemVer mempty str
