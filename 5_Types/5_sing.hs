module Sing where

--fstString :: [Char] -> IO ()
--fstString x =  x ++ " in the rain"

--sndString :: [Char] -> IO ()
--sndString x = putStrLn $ x ++ " over the rainbow"

--singBoth :: [Char] -> [Char] -> IO ()

-- START: Kubrick-esque horror 
singBoth x y =  do
                fstString x  
                sndString y

sing =  if (x < y) then 
            singBoth x y -- Why does this produce a Kubrick-esque horror
        else undefined
        where x = "Singin" :: [Char]
              y = "Somewhere" :: [Char]

-- END


fstString :: [Char] -> [Char] -- IO ()
fstString x =  x ++ " in the rain"

sndString :: [Char] -> [Char] -- IO ()
sndString x = x ++ " over the rainbow"

-- Vers 2: it sings "singin in the rain"
--sing =  if (x < y) then
--            fstString x
--        else 
--            sndString y
--        where
--            x = "Singin"
--            y = "Somewhere"
--
