module AsPatterns where



-- As-patterns allow us to pattern match on segments, but still
-- refer to the original value

-- Example
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

-- 1) This should return True if (and only if) all the values in
--    the first list appear in the second list, though they need
--    not be contiguous:
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf _ [] = False
isSubseqOf [] _ = True
isSubseqOf qry@(x:xs) (y:ys)
  | x == y = True && (isSubseqOf xs ys)  
  | otherwise = False || isSubseqOf qry ys 


t1_isSubseqOf :: IO ()
t1_isSubseqOf = if isSubseqOf "blah" "bwloaoht"
                then putStrLn "isSubseqOf finds non-contiguous substr"
                else putStrLn "isSubseqOf no gd"

t2_isSubseqOf :: IO ()
t2_isSubseqOf = if not (isSubseqOf "blah" "woot")
                then putStrLn "isSubseqOf returns false if substr not found"
                else putStrLn "isSubseqOf no gd"

t3_isSubseqOf :: IO ()
t3_isSubseqOf = if not (isSubseqOf "blah" "halb")
                then putStrLn "isSubseqOf returns false if substr not in original order"
                else putStrLn "isSubseqOf no gd"


test_isSubseqOf :: IO ()
test_isSubseqOf = do
  t1_isSubseqOf 
  t2_isSubseqOf 
  t3_isSubseqOf

-- 2) Split a sentence into words, then tuple each one with its
--    capitalized form:
capitalizeWords :: String -> [(String, String)]
capitalizeWords str = [(word, toUpper l : ls) | word@(l:ls) <- words str]

