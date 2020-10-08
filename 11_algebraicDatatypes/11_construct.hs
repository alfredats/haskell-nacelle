module Construct where

-- Datatypes for chapter 11.3
data GuessWhat = ChickenButt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst  :: a
                , psecond :: b }
                deriving (Eq, Show)

-----------------------------------------------------------
-- Product Types
-----------------------------------------------------------

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
newtype NumSheep = NumSheep Int deriving (Eq, Show)

-- This is a product type
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq,Show)
-- This is equivalent (we defined the Product type above)
type Farmhouse' = Product NumCow NumPig

-- We can nest product types
--   * nested product types
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep
                  deriving (Eq, Show)
--   * equivalent but not typically used
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)
myFarmhouse = Product (NumCow 10) (Product (NumPig 5) (NumSheep 15))                   


-----------------------------------------------------------
-- Sum Types
-----------------------------------------------------------
type Name = String
type Age = Int 
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age 
  deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud 
  deriving (Eq,Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

-- Sum type describing all the animals in the farm
data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo
            deriving (Eq, Show)
mary' = SheepInfo "mary" 3 10
mary  = Sheep mary' -- :: Animal

-- equivalent (but not typically used)
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)
charles' = PigInfo "charles" 2 True
charles = Second (First charles') :: Animal'

-- This will fail because it doesn't match the structure of Animal'
elmo' = Second (SheepInfo "elmo" 5 5)
-- elmo = First elmo' :: Animal' -- uncomment to see the error



-----------------------------------------------------------
-- Constructing Values
-----------------------------------------------------------

-- The Unit Type

-- This is equivalent to writing '()' i.e. a trivial expression
trivialValue :: GuessWhat
trivialValue = ChickenButt 
-- trivial values can signal discrete concepts, without requiring
-- us to flatten to '()' a.k.a the unit type.


-- Unary Type

-- data Id a = MkId a deriving (Eq, Show) -- defined earlier above
-- The unary type constructor, Id, takes an argument. We have to 
-- apply it to something before we can construct a value of the Id type
idInt :: Id Int
idInt = MkId 10


-- Product type

-- These are type synonyms, defined by the 'type' keyword
type Awesome = Bool
-- type Name = String -- defined earlier above
person :: Product Name Awesome -- Product was defined earlier above
person = Product "Simon" True
-- Type synonyms don't obligate us to change our terms, and allow us a 
-- quick and painless way to construct the values we need.


-- Sum type

data Twitter = Twitter deriving (Eq, Show)
data AskFM = AskFM deriving (Eq, Show)
type SN = Sum Twitter AskFM
socialNetwork :: SN 
socialNetwork = First Twitter

-- socialNetwork' :: SN
-- socialNetwork' = Second Twitter -- This fails
-- The failure is because order has to be preserved with sum types.
-- i.e. Twitter is First, & AskFM is Second

-- Actual syntax
data SocialNetwork = Twitter'        -- order is preserved
                   | AskFM'
                   deriving (Eq, Show) 


-- Record Syntax

-- ':t RecordProduct' yields 
--      'RecordProduct :: a -> b -> RecordProduct a b'
-- ':t Product' yields
--      'Product :: a -> b -> Product a b'

-- Record syntax constructs values of products similarly to normal
-- product type syntax. What is different is that record syntax 
-- additionally creates field references for convenience.

type IntFloat = RecordProduct Integer Float -- synonym for convenience
myRecord :: IntFloat
myRecord = RecordProduct 42 0.01
-- this is equivalent
myRecord' = RecordProduct { pfirst = 42
                          , psecond = 0.01 } 


-----------------------------------------------------------
-- Accidental bottoms from records 
-----------------------------------------------------------

-- partial definition of product types can lead to errors
partialRecord :: IntFloat
partialRecord = RecordProduct { pfirst = 420} 
-- this yields a 'not initialized' warning
-- if ignored,
--    calling 'partialRecord' on itself in  REPL throws a 
--    'missing field' exception

-- we're allowed partial applications of data constructors for 
-- "propagation" through programs. But it should be something that is 
-- used warily.




