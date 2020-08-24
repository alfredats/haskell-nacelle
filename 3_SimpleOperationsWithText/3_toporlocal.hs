module TopOrLocal where

-- Concepts:
--  1)  Top-level does not mean top of the file
--  2)  Local defitions are nested within some other expressions
--  3)  Local defintions are not visible outside of the expressions they
--      are defined in
--  4) 'let' and 'where' are how local bindings and declations are
--      defined

toplevelFunction :: Integer -> Integer
toplevelFunction x = 
    x + woot + toplevelValue
        where woot :: Integer -- this is a local definition, 
              woot = 10       -- specific to the function

toplevelValue :: Integer -- this is a top-level definition
toplevelValue = 5
