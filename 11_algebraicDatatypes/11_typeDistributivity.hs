module TypeDistributivity where

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

-- Recall that we cannot permit only one type inhabitant in data 
-- constructor declarations (e.g. You cannot use only the False of Bool,
-- you necessarily have to use Bool in your data constructor declaration)

 -- the type constructors are the arguments here
 -- This is a sum type
data BookType = FictionBook Fiction 
              | NonfictionBook Nonfiction
              deriving Show


type AuthorName = String
-- This declaration is not in normal form (not a sum of products in types)
data Author = Author (AuthorName, BookType)

-- Here we are taking the sumtype BookType and breaking its values out
-- to create a sum of products (which is in normal form)
data Author' = Fiction' AuthorName
             | Nonfiction' AuthorName
             deriving (Eq, Show)



