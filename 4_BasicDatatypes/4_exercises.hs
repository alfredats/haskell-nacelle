-- Exercise: Mood Swing
data Mood = Blah | Woot deriving Show

--  1) Mood
--  2) Blah or Woot
--  3) It should be 'Mood -> Mood'. Data constructors should not be
--     specified in type signatures
--  4)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah
-- Defining the output of a function by matching on its possible data
-- constructors is known as pattern matching. With this function,
-- because it is binary (on top of the binary values of Mood), we can
-- cover all possible cases. However, for other types, there might be a
-- need to use the '_' catch-all "otherwise" case.


-- Exercise: Find the mistakes
--  1) not True && True
--      - This expression returns 'False'
--      - Note that functions have higher precedence than operators, so
--        this expression is simplified as follows '(not True) && True'
--      - Likewise, 'not False && True' returns 'True'
--  2) not (x = 6)
--      - This expression does not compile
--      -  '=' is the assignment operator. We should be using '==' for
--          equality instead.
--  3) (1 * 2) > 5
--      - This expression returns 'False'
--      - Parentheses ensure that '1 * 2' are evaluated first
--      - It is interesting to note however that the parentheses are
--      actually superfluous here, because '*' has a precedence of 7
--      compared to 4 of '>'. Which means '1 * 2' will be evaluated
--      first anyway
--  4) [Merry] > [Happy]
--      - The use of capital letters in the naming of 'Merry' and
--      'Happy' suggest that they might be strings (although it is
--      possible that they are type signatures or module names as well)
--      - Given that type signatures and modules can't be compared (Is
--      this actually true?), we work with the understanding that they
--      are strings.
--      - Therefore we change the expression to '"Merry" > "Happy"'
--      instead, which returns True
--  5) [1, 2, 3] ++ "look at me!"
--      - This does not compile.
--      - ':t (++)' returns '(++) :: [a] -> [a] -> [a]'
--      - This implies that the lists used as arguments should be of the
--      same type.
--      - Change the square brackets to double quotes for the expression
--      to compile 


-- Chapter Exercises
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

--  1) 'length :: [a] -> Int' 
--  2)  a) 5
--      b) 3
--      c) 2 
--      d) 5
--  3) '6 / 3' works. '6 / length [1, 2, 3]' fails to compile because
--      'length [1, 2, 3]' returns an Int, which has no instance of
--      Fractional which is required for '/'.
--  4) 'div 6 $ length [1,2,3]' works. 'div' takes in either a Real type
--      or Enum type. Both Real and Enum have implementations for Int.
--  5)  Bool, False
--  6)  a) Num, no return value
--      b) Bool, False
--  7)  a) Works. Functions have higher precedence than operators so it
--         evaluates to '2 == 2' which is True
--      b) Compile error. Lists can only take 1 type.
--      c) Works. 'length' returns type Int, '+' is defined in typeclass
--         Num, which is a superclass of Int. Thus, Int should have an
--         implementation of '+'
--      d) Returns False. 'b' < 'a' returns False which renders the
--         logical conjunction False.
--      e) Compile Error. There is no implementation of Bool for Num.
--  8)
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

--  9)
myAbs :: Integer -> Integer
myAbs x = if (x >= 0) then x else abs x

--  10) Recall that tuples can be used in type signatures as well
f :: (a, b) -> (c, d) -> ((b, d), (a, c))  
f tup1 tup2 = ((snd tup1, snd tup2), (fst tup1, fst tup2))


-- Correcting Syntax
--  1) f xs = w `x` 1 where w = length xs 
--      Capital letters should be reserved for data constructors and
--      module names 
--  2) (\x -> x)
--      Apparently this is how anonymous functions are written in
--      Haskell. Applying the expression to any literal value will
--      return the literal value
--  3) f (a,b) = a

-- Matching function names to types
--  1) c
--  2) b
--  3) a
--  4) d
