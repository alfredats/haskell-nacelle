--What are types for?

--  Type systems in logic & math were designed to impose constraints
--  that enforce correctness. This eliminates some classes of errors, on
--  top of concerns over the effect of a conditional over a non-Boolean
--  value might be. This enables associations between different parts of
--  a program to fit together in a logically consistent, probably
--  correct way.
--
--  Haskell implements static types, and type-checking occurs at compile
--  time (before running the program). This prevents unexpected results 
--  that make it through the program over runtime. 


--Type Signatures

--  We've come across type signatures several times over the past few
--  chapters, but are taking the time to address something about literal
--  values. When querying the types of literal numeric values, we see
--  type class information instead of a concrete type. This is because
--  of the polymorphic nature of the literal values. Depending on the
--  function implementing the literal value (e.g. a number has typeclass
--  Num), it can take several types ('/' takes numeric values as 
--  Fractional types, while 'div' takes Integral types) 
--
--  To coerce a concrete type, we can do 'expr :: type'. For example,
--  '13 :: Integer' gives the type Integer to 13 within the expression.


--Function Types

--  Functions use '->' as their type constructor. It takes two
--  arguments, one input and one output - as a function should. Note
--  however that it doesn't have a data constructor. 
--
--  [Recap]
--  *   A data constructor is a "function" that takes 0 or more values
--      and returns a new value.
--  *   A type constructor is a "function" that takes 0 or more types
--      and returns a new type.
--
--  Because Haskell has algebraic variables, information about types
--  within the input and outputs of the functions are preserved. 
--
--  KEY TAKEAWAY: '->' is a TYPE constructor for functions.


--Typeclass-constrained type variables

--  The compiler will give the least specific and most general type it
--  can, to maximize applicability of a function. What allows this to
--  happen is the existence of typeclass-constrained polymorphic type
--  variables.
--
--  The basic idea here is that each typeclass contains a standard set
--  of functions that can be used across several concrete types. The
--  type variable constrained by the typeclass can represent any of the
--  concrete types that have instances of that type class. And specific
--  operations on which the function depends are defined for it.
--
--  One thing to note about typeclass-constrained type variables, is
--  that once they are instanced into a concrete type, they no longer
--  become polymorphic. As such, you might see multiple typeclass
--  constraints on one or more variables in type signatures. 
--
--  For example, '(Num a, Num b) => a -> b -> b' 
--           or, '(Ord a, Num a) => a -> a -> Ordering'
--
--  The constraints here (left of the type class arrow '=>') look 
--  like tuples, but they don't have value or term-level effect.


--Currying

-- Just like functions in lambda calculus only take one arguemtn, 
-- Haskell has no inbuilt support for multi-argument functions. What
-- exists is syntactic convenience that construct "curried" functions.
--
-- This is exemplified in the type-signatures of functions. Recall that
-- the type constructor of functions '->' takes one input and one
-- output. When constructing a function that requires multiple
-- parameters, Haskell takes the approach of lambda calculus, and nests
-- the individual single-input arguments.
--
-- Example: 
-- 'a -> a -> a' describes a function that takes 2 arguments of type a
-- and returns a value of type a. The first '->' type constructor takes
-- the first argument, and returns a function which is represented by 
-- the second '->'. A clearer way of looking at it would be as follows
-- 'a -> (a -> a)' due to the right associativity of '->'.
--
-- KIV: Associativity does not change predence or order of evaluation.
--


--Partial Application

-- Because functions are curried, we are allowed to partially apply
-- functions and name them.
--
-- Example (GHCi)
--      subtractStuff:: Integer -> Integer -> Integer
--      subtractStuff x y = x - y - 10
--
--      subtractOne = subtractStuff 1
--      subtractOne 11
--      > -20


--Manual Currying and Uncurrying

-- Although Haskell curries functions by default, we can choose to
-- uncurry them through the nesting of arguments. 
--
-- E.g. 
--      myPlus :: Num a => (a, a) -> a
--
--  'myPlus' uncurries the '+' operator by taking two arguments
--  (honestly imo it's really considered 1).

--Sectioning 

-- We can assign names to partially applied functions as follows:
--
--      x = (^2)
--      y = (2^)
--      z = 5
--
--      x z -- 5 ^ 2
--      > 25
--      y z -- 2 ^ 5
--      > 32
--
--  Note the difference in how the arguments are applied. Remember this
--  when sectioning other functions like 'elem' (requires use of
--  backticks for order)




















