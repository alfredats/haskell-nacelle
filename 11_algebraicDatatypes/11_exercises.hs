-- Chapter 11: Algebraic Datatypes
import Data.Char -- for capitalizeWords

-- Kinds
--  1. Simply put, kinds are types of types
--  2. We query the kind of an expression using ':k'
--  3. A fully-applied, concrete type shows up as "*" while a type that 
--     is still waiting to be applied shows up as "* -> *".
--  4. Refer to 11_dogTypes.hs for a working comparison.



-- Exercise: Dog Types
-- 1) Doggies is a type constructor that takes a polymorphic variable.
-- 2) * -> *
-- 3) *
-- 4) Num a => Doggies a
-- 5) Husky Integer
-- 6) Mastiff String
-- 7) There exists both a type and data constructor of the same name. 
--    On a term-level it would exist as a data constructor, however.
-- 8) doge -> DogueDeBordeaux doge
-- 9) DogueDeBordeaux String



-- Exercise: Vehicles
--    refer to 11_vehicles.hs



-- Exercise: Cardinality

-- 1) PugType has cardinality 1
-- 2) Airline has cardinality 3
-- 3) 'maxBound :: Int16' yields 32767, 'minBound :: Int16' yields -32768
--    Therefore, cardinality(Int16) = 32767 + 1 + 32768 = 65536
-- 4) cardinality(Int) is bounded, but cardinality(Integer) is not. This
--    behavior is expected given the natures of the respective data 
--    structures. 'Integer' is arbitrarily large while 'Int' has a 
--    fixed precision. (Refer to chapter 4)
-- 5) 8 is the number of bits that Int8 utilizes to store its memory. 
--    Because a bit has binary values (1 or 0), the cardinality of the 
--    Int8 is given by (2^8) = 256.



-- Exercise: For example

-- 1) ':t MakeExample' yields 'MakeExample :: Example'. ':t Example' 
--    throws an error because it is a type already. The appropriate 
--    command in this case is ':k Example' which queries the kind of
--    Example.
-- 2) ':info Example' yields the definitions involving the Example type.
--    There is the definition of the type with respect to the type and 
--    data constructor, as well as an instance defition of Show for 
--    Example, generated from our use of 'deriving Show'.
-- 3) ':t MakeExample' now yields 'Int -> Example'. The data 
--    constructor now takes an argument, before it constructs a data 
--    expression of the Example type.



-- newtype(s)
--  1. newtypes can only have a single unary data constructor
--  2. advantage of newtypes is that 
--      (a) it has no runtime overhead compared to vanilla data 
--          declarations.
--      (b) works similarly to type synonyms, but has more flexibility
--          (refer to 11_newtype.hs)


-- Exercise: Logic Goats
--      refer to 11_logicGoats.hs


-- Exercise: Pity the Bool

-- 1) The data constructors 'Big' and 'Small' are unary data constructors 
--    that take Bool values, the possible term values that can inhibit a
--    BigSmall type are, explicitly, 
--        
--        [Big True, Big False, Small True, Small False]
--
--    Therefore, cardinality(BigSmall) = 4.
-- 2) 256 + 2 = 258



-- Exercise: How does your garden grow?
type Gardener = String
data Garden = Gardenia Gardener -- Gardener <flowername> throws an error 
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show


-- Exercise: Programmer
--      refer to 11_programmer.hs



-- Calculating type inhabitants
--  *  Given a function a -> b, the number of inhabitants of the type
--    is calculated by (b ^ a).
--  * a -> b -> c has ((c ^ b) ^ a) number of possible implementations
--    This can be simplified (with associativity of ^) to c ^ (b * a)
--    (verify with logarithms)
--  * Refer to chapter 11.4 for a full illustration.
--  * Refer to 11_convert.hs for the Quantum -> Bool exercise.


-- Exercise: The Quad 
--  1) 16 forms
--  2) 16 forms
--  3) 256 forms (because function)
--  4) 8 forms
--  5) 16 forms
--  6) ((4 ^ 4) ^ 2) = 65536 



-- Lists are polymorphic
data MyList a = Nil | Cons a (MyList a)

-- Binary Tree 
--      refer to 11_binaryTree.hs



-- Chapter Exercises

-- Multiple Choice
-- 1) a
-- 2) c
-- 3) b
-- 4) c

-- Ciphers
--    refer to 11_cipher.hs

-- As-patterns
--    refer to 11_asPatterns.hs


-- Language Exercises
--  1)  Write a function that capitalizes a word
capitalizeWord :: String -> String
capitalizeWord (' ':xs) = ' ' : capitalizeWord xs
capitalizeWord (x:xs) = toUpper x : xs


--  2)  Write a function that capitalizes sentences in a paragraph. 
--      Recognize when a new sentence has begun by checking for periods.
--      Reuse the capitalizeWord function.
tokenize :: String -> [String]
tokenize = foldr rt []
  where 
    rt c []     = [[c]]
    rt '.' ss   = "." : ss
    rt c (s:ss) = (c : s) : ss

-- foldr rt [] "hi. you" =
-- (h `rt` (i `rt` (. `rt` (' ' `rt` (y `rt` (o`rt`(u`rt`[] )))))))  =
-- (h `rt` (i `rt` (. `rt` (' ' `rt` (y `rt` (o`rt`(('u' : []) : [] )))))))  =
-- (h `rt` (i `rt` (. `rt` (' ' `rt` (y `rt` (('o' : 'u' : []) : [] ))))))  =
-- (h `rt` (i `rt` (. `rt` ((' ' : 'y' : 'o' : 'u' : []) : [] ))))  =
-- (h `rt` (i `rt`("." : " you" : [] )))  =
-- (h `rt` ("i." : " you" : [] ))  =
-- ("hi." : " you" : [] ) 

capitalizeParagraph :: String -> String
capitalizeParagraph = concat . map capitalizeWord . tokenize  






