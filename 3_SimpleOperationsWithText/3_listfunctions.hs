--  Since strings are list of char values, standard list operations can 
--  be used on strings.

--  NOTE: list operations, even though they are standard Prelude 
--  functions, are "unsafe". They do not know how to handle empty lists,
--  and are unwise for use with programs with any size or complexity. 


-- ':' is the cons or constructor operator
fullname = char_eg : str_eg
    where char_eg = 'c'
          str_eg = "hris" -- note the char 'c' vs the string "hris" 


-- 'head' returns the head/first element of a list, 'tail' returns the
--  converse
head "papuchon"
tail "papuchon"


-- 'take' gives a specified number of elements from the list,
-- starting from the left
take 1 "papuchon"
take 0 "papuchon"
take 6 "papuchon"


-- 'drop' does the converse
drop 1 "papuchon"
drop 0 "papuchon"
drop 6 "papuchon"


-- '++'(infix) or 'concat'(prefix) are the concatenation operators
"hello" ++ " " ++ "world!"
concat ["hello", " ", "world!"]


-- '!!' is the indexing function (one index only, see :t (!!) for more)
"papuchon" !! 1
"papuchon" !! 0
"papuchon" !! 6





