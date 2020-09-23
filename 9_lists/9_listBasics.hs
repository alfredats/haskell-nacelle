-- List construction

basicList = [1,2,3]

myList = [1..10]
myList' = enumFromTo 1 10

secList = [1,2..10]
secList' = enumFromThenTo 1 2 10

oddList = [1,3..10]
oddList' = enumFromThenTo 1 3 10

evenList = [2,4..10]
evenList' = enumFromThenTo 2 4 10

charList = ['t'..'z']
charList' = enumFromTo 't' 'z'



-- Pattern Matching

myHead :: [a] -> a
-- myHead []      = [] -- This does not typecheck... why?
myHead (x : _) = x

myTail :: [a] -> [a]
myTail []       = [] -- Pattern match on empty list
myTail (_ : xs) = xs


safeTail :: [a] -> Maybe [a] -- safer due to Maybe datatype
safeTail []     = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x



-- Extracting portions of lists

-- take 3 [1..10]
-- drop 3 [1..10]
-- splitAt 5 [1..10] 

-- takeWhile (<4) [1..10]
-- dropWhile (<4) [1..10]

-- NOTE: takeWhile (>6) [1..10] evaluates to [] because it stops taking
-- as soon as the condition is not met.

-- NOTE: dropWhile (>6) [1..10] evaluates to [1..10] because is stops 
-- dropping as soon as the condition is not met.


-- List Comprehensions

-- Similar to  set comprehensions.
--
-- NOTE: Additional predicates are seperated by commas
--        
--        [x^2 | x <- [1..10], rem x 2 == 0] = [4,16,36,64,100]

-- NOTE: Multi-variable predicates are fulfilled right-to-left
--
--        [x^y | x <- [1,2,3], y <- [2,3]] = [1^2, 1^3, ... , 3^2, 3^3]
--                                         = [1,1,4,8,9,27]



