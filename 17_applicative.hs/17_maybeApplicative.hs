module MaybeApplicative where


-- Imagine a scenario where the following function is use to validate 
-- inputs for a type Maybe Person. Maybe is used because the inputs might
-- be invalid.

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if length s > maxLen
                             then Nothing
                             else Just s


-- Types
newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person -- This causes an typing issue
mkPerson n a = case mkName n of              
                 Nothing -> Nothing
                 Just n' ->
                   case mkAddress a of 
                     Nothing -> Nothing
                     Just a' ->
                       Just $ Person n' a'

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a


