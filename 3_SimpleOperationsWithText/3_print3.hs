module Print3 where

-- Concepts learnt:
--  1)  'do' allows for sequencing actions (to be further elaborated)
--  2)  it is considered best practice to do type declarations for each
--      top level expression
--  3)  values are defined at the top level of a module to allow
--      availability throughout the module
--  4)  concatenate strings with '++' or 'concat'

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where secondGreeting = 
            concat [hello, " ", world]
