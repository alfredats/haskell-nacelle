-- Exercises: Scope
--  1)  y is a top level definition, and is in scope throughout the
--      whole runtime, which include z
--  2)  h is not defined at all. h is not in the scope for g
--  3)  no. the definition for r is a top-level definition, which should
--      not be recursively referencing d, as d is not in the scope of r.
--  4)  yes. the usage of 'where' allows for local definition of
--      variables before the expression is executed.


-- Exercises: Syntax errors
--  1)  '++ [1,2,3] [4,5,6]' will not compile due to the use of '++' in a 
--      prefix format without surrounding backticks. 
--
--      `++` [1,2,3] [4,5,6] will compile.
--
--  2)  '<3' ++ 'Haskell' does not compile due to the use of single
--      quotes to encapsulate strings. Single quotes are used as
--      identifiers for char types (like in c). Strings are lists of
--      chars and thus are encapsulated by " instead.
--
--      "<3" ++ "Haskell" will compile.
--
--  3) concat ["<3", "Haskell"] will compile.


-- CHAPTER EXERCISES: READING SYNTAX

--  1)  Decide, for the following lines of code if they're written
--      correctly
--
--          a) Written correctly
--          b) The '++' should be parenthesised if we want to use prefix
--             syntax. Otherwise if should be between the two lists.
--          c) Written correctly
--          d) If the intention is to return a list containing a single
--             string, insert a double quote before the closing square 
--             bracket.
--          e) The arguments should switch places. '!!' takes the list
--             as first argument, and the index desired as second.
--          f) Written correctly.
--          g) The starting double quote should no encompass the index
--             desired. Correct Syntax: take 4 "hello"
--          h) Written correctly.

--  2) Match the inputs to the expected outputs
--      a) Input{a} -> Output{d}
--      b) {b} -> {c}
--      c) {c} -> {e}
--      d) {d} -> {a}
--      e) {e} -> {b}


-- CHAPTER EXERCISES: BUILDING FUNCTIONS

-- 2) a) 
f2a :: String -> String
f2a x = x ++ "!"
-- f2a "curry is awesome"
-- 2) b)
f2b :: String -> String
f2b x = [x !! 4]
-- f2b "curry is awesome!"
-- 2) c)
f2c :: [a] -> [a]
f2c x = drop 9 x
-- f2c "curry is awesome!"

