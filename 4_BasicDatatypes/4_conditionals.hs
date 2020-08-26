-- Conditionals

-- As with most things Haskell, if-else statements manifest themselves
-- as 'if expressions'. 
--
-- Example:
-- if True 
--   then t 
-- else f 
-- where t = 1
--       f = 2

-- Some things of note:
--  1) the entire expression is type-checked as the type of the return 
--  value.
--  2) Unlike Python, there are no "truthy" or "falsey" values in 
--  Haskell. If the type has no implementation for Bool, the 
--  if-expression will not compile.
--  
