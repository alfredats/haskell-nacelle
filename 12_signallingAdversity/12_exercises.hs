-- Signalling Adversity


-- The Maybe datatype

--    data Maybe a = Nothing | Just a

-- "Maybe" is included in Prelude by default, and allows us to return a 
-- "Nothing" value when there are no sensible values to return for the 
-- intended type.

-- E.g. 
--    ifEvenAdd2 :: Integer -> Maybe Integer
--    ifEvenAdd2 n =
--      if even n then Just (n + 2) else Nothing
--      


-- When writing functions that construct values of a certain type (smart 
-- constructors), the "Maybe" datatype can be used to check for adverse 
-- data. 

-- E.g. 
type Name = String
type Age = Integer
data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age 
  | name /= "" && age >= 0 = -- Condition to check for valid input
      Just $ Person name age
  | otherwise = Nothing



-- The "Either" datatype

-- Taking the previous example further, suppose we want to know why a 
-- function does not return a successful result. We can do this with the 
-- "Either" datatype.

--    data Either a b = Left a | Right b

-- We make a sum type to enumerate the failure modes of "mkPerson"

data PersonInvalid = NameEmpty
                   | AgeTooLow 
                   deriving (Eq,Show)

-- It is important to remember that guards using equality checks require
-- the derivation of "Eq". Case expressions and pattern matching will 
-- work without an instance of "Eq", but guards using "==" will not.


mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age
  | age < 0    = Left AgeTooLow
  | name == "" = Left NameEmpty
  | otherwise = Right $ Person name age

-- By convention, "Left" is used as the invalid or error constructor. 
-- This is because of (1) the ordering of type arguments, and (2) how 
-- Functors behave which will be explained in a later chapter.


-- Finally, we give an example of a fully implemented type system, 
-- complete with error-checking functions, which return a list of errors.
--      refer to 12_mkPerson.hs for the full implementation


-- Chapter Exercises

-- Determine the Kinds
--
-- 1) id :: *
-- 2) a :: * , f :: * -> *
--
--
-- String Processing
--    refer to 12_StringProcessing.hs
--
--
-- Validate the word
--    refer to 12_validateWord.hs
--
--
-- It's only Natural
--    refer to 12_natural.hs
--
--
-- Small library for Maybe
--    refer to 12_maybeLibrary.hs
--
--
-- Small library for Either
--    refer to 12_eitherLibrary.hs
--
--
-- Unfolds
--    refer to 12_unfolds.hs
--
-- Something other than a list!
--    refer to 12_binaryTree.hs
