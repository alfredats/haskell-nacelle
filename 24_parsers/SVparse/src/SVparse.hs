module SVparse where

-- TODO :: parseSemVer currently fails to parse metadata

import Text.Trifecta
import Control.Applicative

data NumberOrString = NOSS String
                    | NOSI Integer
                    deriving (Eq, Ord, Show, Read)

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

parseMMP :: Parser MMP
parseMMP = do
  vMaj <- integer
  _ <- char '.'
  vMin <- integer
  _ <- char '.'
  vPatch <- integer
  return (vMaj, vMin, vPatch) 

parseNOSI :: Parser NumberOrString 
parseNOSI = do
  skipMany (char '.')
  nums <- some digit
  skipMany (char '.')
  return (NOSI $ read nums)
   
parseNOSS :: Parser NumberOrString 
parseNOSS = do
  skipMany (char '.')
  NOSS <$> some letter
    
parseRelMeta :: Parser [NumberOrString]
parseRelMeta = many ((parseNOSI <?> "try NOSI") 
  <|> (parseNOSS <?> "try NOSS"))

parseSemVer :: Parser SemVer
parseSemVer = do
  (major, minor, patch) <- parseMMP
  hasRelease <- Left <$> eof <|> Right <$> char '-'
  case hasRelease of 
    Right '-' -> do
      vRel <- parseRelMeta 
      hasMeta <- Left <$> eof <|> Right <$> char '+'
      case hasMeta of
        Right '+' -> do
          metaData <- parseRelMeta
          return $ SemVer major minor patch vRel metaData
        _ -> return $ SemVer major minor patch vRel [] 
    _   -> return $ SemVer major minor patch [] []
  
pMMP :: String -> IO ()
pMMP str = print $ parseString parseMMP mempty str


psv :: String -> IO ()
psv str = print $ parseString parseSemVer mempty str


test :: IO ()
test = do
  psv "2.1.1"
  psv "1.0.0-x.7.z.92"
