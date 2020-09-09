--Chapter exercise: Eq Instances
--  1)
data TisAnInteger = 
    TisAn Integer

instance Eq TisAnInteger where 
    (==) (TisAn integer)
         (TisAn integer') = 
            (integer == integer')

--  3)
data StringOrInt =
        TisAnInt    Int
      | TisAString  String  -- Are concrete type constraints enforced?
                            -- or are these just names?

instance Eq StringOrInt where
    (==) (TisAnInt int)
         (TisAnInt int') = 
             (int == int')
    (==) (TisAString string)
         (TisAString string') = 
             (string == string')
    (==) _ _ = False

-- 5)
data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') =
        (x == x') && (y == y')

-- 7)
data EitherOr a b =
        Hello   a
      | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x') = x == x'
    (==) (Goodbye y) (Goodbye y') = y == y'
    (==) _ _ = False    


--Chapter Exercise: Tuple experiment
-- divMod :: a -> a -> (a, a)
-- quotRem:: a -> a -> (a, a)
--      Given the type signatures of divMod and quotRem, they likely
--      take two arguments that semantically represent the dividend &
--      divisor. The outputs should be (quotient, remainder), with the
--      quotient rounded towards -inf in the case of divMod and towards
--      0 in the case of quotRem.
--


--Chapter Exercise: Put on your thinking cap
--  Any function that requires Fractional necessarily requires Num. This
--  is because the set of Fractional is a proper subset of the set of
--  Num.


--Chapter Exercise: Will they work?
--  1) Should work. The two seperate length exprs are encapsulated by
--  parentheses which mean they will return Int types, that aare 
--  then fed as arguments for the 'max' function. Since 'max' has the
--  typeclass constrain of 'Ord a =>', and there is an instance of Ord
--  for Int, the expression compiles and returns the result 5.
--  
--  2)
--  
--  3) Expr does not compile. "Julie" is String type while 'True' is
--  Bool type. The first argument dictates what concrete type the
--  function should implement, which requires the second argument in
--  this case to be also of String type. 


--Exercises:
--  Multiple Choice
--      1) c
--      2) b
--      3) a
--      4) c
--      5) a

--  Does it type check?
--      1) Nope. There is no instance of show for the type Person. 
--         To fix, use 'data Person = Person Bool deriving Show', or
--         write an instance of Show for Person.
--      2) No! There isn't an instance of Eq for type Mood. To fix, use
--         'data Mood = Blah | Woot deriving (Eq, Show)'
--      3)  a) Only Blah or Woot
--          b) Compile error! There isn't an instance of Num for Mood
--          c) Compile error! There isn't an instance of Ord for Mood
--      4) s1 does not typecheck but s2 does. s1 is a partial
--         application of the Sentence type constructor, and requires 
--         an Object input.

--  Given a datatype declaration, what can we do?
--      1) Does not typecheck! The Papu type constructor takes arguments
--         of type Rocks and Yeah, which each have their specific type
--         constructors.
--      2) Typechecks.
--      3) Typechecks. Papu, as well as its constituent types Rocks
--         and Yeah derive Eq instances, allowing for '==' operations.
--      4) Does not typecheck! There is no Ord instance for Papu. Eq is
--         a superclass for Ord, and does not contain the methods 
--         required for '>'.

--  Match the types
--   
--   Remember that haskell is static-ly typed. There can be no ambiguity
--   or the typechecker will be upset.
--
--      1) hells naw. a is polymorphic and not a typeclass or concrete 
--      type. 
--      2) nope. '1.0' is Fractional. There is no instance of Num for 
--      Fractional types.
--      3) Yes. In this case, the initial type definition was a concrete
--      subclass of Fractional. The new type definition is less specific,
--      but it still works.
--      4) Yes. Similar to 3 but the new type definition is for RealFrac
--      5) Can substitute. Technically there isn't any comparison within
--      the 'freud' function, so the Ord typeclass constrain is 
--      superfluous.
--      6) Can substitute. Same as (5)
--      7) Nope. In this case, using 'myX' in the definition of 'sigmund'
--      makes it such that the output value can only be of type 'Int'.
--      8) Nope, same as above.
--      9-10) Not sure what i'm looking at. will revisit.

-- TypeKwonDo 2
--      I kinda struggle with this quite a bit...
      
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk ab a b = (ab a) == b

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith ab _ a = ab a
