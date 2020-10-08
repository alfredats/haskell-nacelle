module Deconstruct where

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

-- FarmerType is a sum of types
data FarmerType = DairyFarmer 
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

-- Farmer is a product of types
data Farmer = Farmer Name Acres FarmerType
            deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False 



-- ALTERNATIVE FORMULATION
data FarmerRec = 
  FarmerRec { name :: Name
    , acres :: Acres
    , farmerType :: FarmerType } 
    deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = 
  case farmerType farmer of -- case statements allow pattern matching
    DairyFarmer -> True     -- of data constructors without Eq
    _           -> False

--this requires an Eq typeclass constrain.
--isDairyFarmerRec' :: FarmerRec -> Bool
--isDairyFarmerRec' farmer
--  | farmerType farmer == DairyFarmer = True
--  | otherwise = False 


-------------------------------------------------------------
-- Accidental Bottoms from sums of records
------------------------------------------------------------
-- Similar to the demonstration in 11_construct.hs, we can 
-- unknowingly propagate bottoms through sums of types.
data Automobile = Null 
                | Car { make :: String
                      , model :: String
                      , year :: Integer }
                deriving (Eq, Show)

-- 'make Null' will throw an exception

-- The appropriate way to do this is to "abstract" the higher order 
-- sum of types from the record type constituent.
data Car' = Car' { make' :: String
                 , model'  :: String
                 , year' :: Integer }
                 deriving (Eq, Show)

data Automobile' = Null'    -- should actually use the "Maybe" type 
                 | Automobile' Car'
                deriving (Eq, Show)

-- This formulation allows the typechecker to catch and identify the 
-- appropriate mistake. (compare 'make Null' from abv to 'make Null'')
