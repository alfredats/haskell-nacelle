module ProductTypes where

-----------------------------------------------------------
-- PRODUCT TYPES 
-----------------------------------------------------------
data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth
                 deriving (Eq, Show)

-- MkTwoQs -> data constructor that is a product type & has cardinality 6
data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)

-- Using type alias to construct a product type
type TwoQs' = (QuantumBool, QuantumBool)


-----------------------------------------------------------
-- RECORD SYNTAX
-----------------------------------------------------------
-- Using "typical" syntax
data Person = MkPerson String Int deriving (Eq, Show)
namae :: Person -> String
namae (MkPerson s _) = s

jm = MkPerson "julie" 108
-- print (namae jm) -> yields "julie"

-- Using "record" syntax
data Person' = Person' { name :: String
                       , age  :: Int }
                       deriving (Eq, Show)

papu = Person' "Papu" 5
-- 'name' and 'age' become named record field accessors i.e they become
-- functions that have the type "Person -> String" and "Person -> Int" 
-- respectively.
--      age papu -> yields 5
--      name papu -> yields "Papu"
--
