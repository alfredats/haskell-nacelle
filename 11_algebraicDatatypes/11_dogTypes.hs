module DogTypes where

data PugType              = PugData
-- The type argument 'a' is a phantom and has no witness
data HuskyType a          = HuskyData 
data DogueDeBordeaux doge = DogueDeBordeaux doge


myPug = PugData :: PugType

myHusky :: HuskyType a 
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[Int]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

badDoge :: DogueDeBordeaux String
badDoge = DogueDeBordeaux "10"


-- Kind signatures vs Type signatures
data Doggies a = 
    Husky a
  | Mastiff a 
  deriving (Eq, Show)

-- ':k Doggies' yields "Doggies :: * -> *" (Kind signature)
-- ':t Husky' yields "Husky :: a -> Doggies a" (Type signature)
