module HuttonsRazor where

data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Add x y) = (eval x) + (eval y)
eval (Lit x) = x 

test_eval :: IO ()
test_eval = if (eval (Add (Lit 1) (Lit 9001))) == 9002
            then putStrLn "test_eval ok"
            else putStrLn "test_eval no okay"


printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = (printExpr x) ++ " + " ++ (printExpr y)

test_printExpr = do
  if (printExpr a1) == "9001 + 1" 
     then putStrLn "printExpr test 1 pass"
     else putStrLn "printExpr test 1 nope"
  if (printExpr a3) == "1 + 9001 + 1 + 20001"
     then putStrLn "printExpr test 2 pass"
     else putStrLn "printExpr test 2 nope"
  where
    a1 = Add (Lit 9001) (Lit 1)
    a3 = Add (Lit 1) (Add a1 (Lit 20001))

main :: IO ()
main = do
  test_eval
  test_printExpr 
