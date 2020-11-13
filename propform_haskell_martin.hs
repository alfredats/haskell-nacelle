module Propositional where

import Test.Hspec
import Test.QuickCheck
import Data.List

data Form =
    C Bool 
  | V String 
  | Not Form
  | Form `Or` Form
  | Form `And` Form
  deriving (Eq, Ord, Show, Read)

formBoolGen :: Gen Form
formBoolGen = elements [C True, C False]

formStringGen :: Gen Form
formStringGen = fmap (V . flip (:) []) $ elements ['a'..'z']

validityGen :: Gen [Form]
validityGen = sequence [formBoolGen, formBoolGen]

formGen :: Gen Form
formGen = frequency [ (2, fmap Not $ formGen)
                    , (2, formBoolGen)
                    , (2, formStringGen) 
                    , (2, orGen)
                    , (2, andGen) ]
            where
              orGen :: Gen Form
              orGen  = do
                x' <- formGen
                y' <- formGen
                return (x' `Or` y')
              andGen :: Gen Form
              andGen = do
                x' <- formGen
                y' <- formGen
                return (x' `And` y')

instance Arbitrary Form where
  arbitrary = formGen 

-- Semantic Equivalence 
removeConst :: Form -> Form
removeConst (f `And` (C True)) = removeConst f
removeConst (_ `And` (C False)) = C False
removeConst ((C True) `And` f) = removeConst f
removeConst ((C False) `And` _) = C False
removeConst (_ `Or` (C True)) = C True
removeConst (f `Or` (C False)) = removeConst f
removeConst ((C True) `Or` _) = C True
removeConst ((C False) `Or` f) = removeConst f
removeConst (Not (C True)) = C False
removeConst (Not (C False)) = C True
removeConst f = f


simplifyConst :: Form -> Form
simplifyConst (Not f) = 
  removeConst (Not (removeConst f)) 
simplifyConst (f1 `And` f2) = 
  removeConst ((removeConst f1) `And` (removeConst f2))
simplifyConst (f1 `Or` f2) =
  removeConst ((removeConst f1) `Or` (removeConst f2))

-- Negation Normal Form
nnf :: Form -> Form 
nnf (Not (Not f)) = f
nnf (Not (f1 `Or` f2)) = (Not f1) `And` (Not f2)
nnf (Not (f1 `And` f2)) = (Not f1) `Or` (Not f2)

-- Conjunctive Normal Form
distribOr :: Form -> Form -> Form
distribOr (a `And` b) (c `And` d) = 
  (distribOr (a `And` b) c) `And` (distribOr (a `And` b) d)
distribOr (a `And` b) c = (a `Or` c) `And` (b `Or` c)
distribOr a (b `And` c) = (a `Or` b) `And` (a `Or` c)
distribOr a b = a `Or` b


cnf :: Form -> Form
cnf (f1 `Or` (f2 `And` f3)) = distribOr f1 (f2 `And` f3)
cnf ((f1 `And` f2) `Or` f3) = distribOr (f1 `And` f2) f3



-- Auxillary Functions

fvList :: Form -> [String]
fvList (f1 `Or` f2) = nub ((++) (fvList f1) (fvList f2)) 
fvList (f1 `And` f2) = nub ((++) (fvList f1) (fvList f2)) 
fvList (Not f) = fvList f
fvList (V x) = [x]

subst :: Form -> (String, Bool) -> Form
subst (f1 `And` f2) s = (subst f1 s) `And` (subst f2 s)
subst (f1 `Or` f2) s = (subst f1 s) `Or` (subst f2 s)
subst (Not f) s = Not (subst f s)
subst f@(V v) (x,y) = if v == x then C y else f
subst f _ = f

substAll :: Form -> [(String, Bool)] -> Form
substAll expr subList = foldl (subst) expr subList 

evalSubst :: Form -> [(String, Bool)] -> Bool
evalSubst expr subList 
  | x == C False = False
  | x == C True = True
  | otherwise = evalSubst x subList
    where x = simplifyConst (substAll expr subList)


-- Model Finding
models :: Form -> [(String, Bool)] -> [String] -> [[(String, Bool)]]
models f vl [] = if evalSubst f vl == True then [vl] else []
models f vl (vn:vns) = 
  (++) (models f ((vn, True):vl) vns) (models f ((vn, False): vl) vns)


allModels :: Form -> [[(String, Bool)]]
allModels f = models f [] $ fvList f

unsatisfiable :: Form -> Bool
unsatisfiable f = if allModels f == [] then True else False

valid :: Form -> Bool
valid f = if unsatisfiable (Not f) then True else False


expr = (V "a" `Or` (Not (V "a")))


-- Tests

tests :: IO ()
tests = hspec $ do
  describe "simplifyConst" $ do
    it "((V \"a\") `And` (C False `Or` C True))" $ do
      simplifyConst ((V "a") `And` (C False `Or` C True)) `shouldBe` V "a"
  describe "nnf" $ do
    it "Not Not f == f" $ do
      nnf (Not (Not (V "a"))) `shouldBe` (V "a")
    it "Not (f `And` g) == (Not f) `Or` (Not g)" $ do
      nnf (Not (V "f" `And` V "g")) `shouldBe` (Not (V "f") `Or` Not (V "g"))
    it "(f `Or` g) == (Not f) `And` (Not g)" $ do
      nnf (Not (V "f" `Or` V "g")) `shouldBe` (Not (V "f") `And` Not (V "g"))
  describe "cnf" $ do
    it "(a `And` b) `Or` (c `And` d)" $ do
      cnf ((V "a" `And` V "b") `Or` (V "c" `And` V "d")) `shouldBe`
        ((V "a" `Or` V "c") `And` (V "b" `Or` V "c")) `And` 
          ((V "a" `Or` V "d") `And` (V "b" `Or` V "d"))
  describe "evaluating boolean formulas" $ do
    it "validity : a `Or` b `Or` Not a" $ do
      forAll validityGen (\[a,b] -> simplifyConst (a `Or` b `Or` Not a) == C True) 
  describe "fvList" $ do
    it "fvList (V \"a\" `And` (Not (V \"a\") `Or` (Not (V \"b\"))))" $ do
      fvList (V "a" `And` (Not (V "a") `Or` (Not (V "b")))) `shouldBe` 
        ["a", "b"]
  describe "subst" $ do
    it "subst (V \"a\") (\"a\", True) == C True" $ do
      subst (V "a") ("a", True) `shouldBe` C True
    it "subst (V \"a\") (\"a\", False) == C False" $ do
      subst (V "a") ("a", False) `shouldBe` C False
    it "subst x (\"a\", True) == x" $ do
      forAll formBoolGen (\s -> subst s ("", True) == s)
  describe "validity function" $ do 
    it "valid (V \"a\" `Or` (Not (V \"b\"))) == False" $ do
      valid (V "a" `Or` (Not (V "b"))) `shouldBe` False
    it "valid (V \"a\" `Or` (Not (V \"a\"))) == True" $ do
      valid (V "a" `Or` (Not (V "a"))) `shouldBe` True
  describe "unsatisfiability function" $ do
    it "unsatisfiable (V \"a\" `Or` (Not (V \"b\"))) == False" $ do
      unsatisfiable (V "a" `Or` (Not (V "b"))) `shouldBe` False
    it "unsatisfiable (V \"a\" `Or` (Not (V \"a\"))) == False" $ do
      unsatisfiable (V "a" `Or` (Not (V "a"))) `shouldBe` False

