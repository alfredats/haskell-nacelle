module MaybeLibrary where

-- 1) Simple Boolean checks for Maybe values

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ =False


-- 2) Catamorphisms for Maybe

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing  = x
mayybee _ f (Just y) = f y 


-- 3) In case you want to provide a fall back value. Try writing it in 
--    terms of the maybe catamorphism

fromMaybe :: a -> Maybe a -> a
fromMaybe x y = mayybee x (id) y


-- 4) Converting between List and Maybe

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]


-- 5) For dropping Nothing values from a list
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of 
                     Nothing  -> catMaybes xs
                     (Just y) -> y : catMaybes xs

-- 6) 
--    This solution is thanks to johsi. My first attempts at solving this
--    was to look at the 'sequence' function which was mentioned in the
--    question. However, this lead me down a rabbit hole of functors and
--    Traversables which I couldn't understand at all. After an hour or 
--    so of being stuck, I decided to refer to johsi's answers and 
--    realised that the solution was such a simple and elegant one.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr rf (Just [])
  where
    rf Nothing _ = Nothing
    rf (Just _) Nothing = Nothing
    rf (Just x) (Just acc) = Just (x: acc)

