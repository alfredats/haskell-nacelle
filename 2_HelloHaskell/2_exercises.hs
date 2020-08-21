-- In-text exercises
--  Comprehension Check
half x =
    x / 2

square x =
    x * x

areaCircle x =
    pi * square x

--  Exercise: Parentheses and Association
--  1)  We expect parentheses to change the results.
--      '*' has a precedence of 7 vs '+' which has precedence of 6.
-- 
--       8 + 7  * 9 = 71
--      (8 + 7) * 9 = 135 
-- 
--  2)  Parentheses will not change the results. Precedence in both cases is preserved.
--  3)  Parentheses will change the results.
--      '/' has precedence 7 vs '+' which has precedence 6. 


--  Exercise: Heal the Sick
--  1)  area x = 3. 14 * ( x * x )
area x = 3.14 * ( x * x )

--  2)  double x = b * 2
double x = x * 2

--  3)  x = 7
--       y = 10
--      f = x + y 
x = 7
y = 10
f = x + y 


-- Chapter Exercises
--  Parenthesization
2 + (2 * 3) - 3
(^) 10 $ (1 + 1)
(2 ^ 2) * (4 ^ 5) + 1

--  Equivalent Expressions
--  1) 1 + 1 and 2 are equivalent

--  2) 10 ^ 2 and 10 + 9 * 10 are equivalent
--      10 + 9 * 10 = 10 + (9 * 10) = 100

--  3) 400 - 37 and (-) 37 400 are not equivalent.
--      '-' is an infix operator, and is coerced into prefix behavior
--      by wrapping it in parentheses. The latter expression yields 
--      37 - 400 which returns -363. The former returns 363. Note
--      however that using the prefix operator 'subtract' i.e. 'subtract
--      37 400' instead returns the same result as the former. It is
--      best to reserve the '-' operator for negation.

--  4) 100 `div` 3 and 100 / 3 are not equivalent.
--      100 `div` 3 is integral division rounded to -inf. 
--      100 / 3 is fractional division which yields a float.

-- More fun with functions
--      see 2_morefunwithfunctions.hs
