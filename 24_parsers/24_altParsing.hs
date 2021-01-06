module AltParsing where

import Control.Applicative
import Text.Trifecta

type NumberOrString = Either Integer String 

a = "blah"
b = "123"
ab = "123blah789"

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

main = do
  let p f i = parseString f mempty i
  print $ p (some letter) a
  print $ p (integer) b
  
  print $ p parseNos a
  print $ p parseNos b

  print $ p (many parseNos) ab
  print $ p (some parseNos) ab

--  The existence of <|>, some and many owe their existence 
--  to the Alternative typeclass. 
--
--  A note on some and many:
--    
--  * some and many are defined recursively by each other,
--    and the only difference between them is the order
--    at which the constituent recursive functions are 
--    applied.
--  
--  * defns :
--      
--      some v = some_v
--      many v = many_v
--        where
--          many_v = some_v <|> pure []
--          some_v = (fmap (:) v) <*> many_v
--
--  * with the some function, because it starts with the 
--    fmap operation, it necessitates one or more of the 
--    parser succeeding. With the many function, because 
--    the first step already contains parser disjunction,
--    it is allowed to fail and return an empty list.
--
--  * Specifically, some recursively calls some_v and feeds
--    the fmap function until the text input is fully parsed,
--    afterwhich it fails, and the pure [] is returned.
--
--      parsing "ab" with some_v, step by step 
--      
--      fmap (:) letter :: CharParsing f => f ([Char] -> [Char])
--      
--      ((fmap (:) v) <*> many_v) "ab" = 
--      ((fmap (:) v) <*> some_v                    <|> pure []) "ab" =
--      (...          <*> ((fmap (:) v) <*> many_v) <|> pure []) "ab" = 
--      (...          <*> ((fmap (:) v) <*> many_v) <|> pure []) "ab" = 
--      
--      The fact that it returns the string in original form instead of
--      reversed makes me think that function application is occuring 
--      from within nested parentheses outwards. 
--      However, that would imply that parsing happens from eof and 
--      proceeds backwards. Which is surely wrong, right?
--      
--      update: letter uses tokenization, which affects the way that
--              the string is parsed. Still doesn't really answer
--              the question though.
--
--
--
