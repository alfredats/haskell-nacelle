data Quantum = Yes
             | No
             | Both
             deriving Show


convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = True
convert2 Both = False 

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False 
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes  = False
convert4 No   = True 
convert4 Both = True 

convert5 :: Quantum -> Bool
convert5 Yes  = True
convert5 No   = False
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes  = False
convert6 No   = False
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes  = False
convert7 No   = True
convert7 Both = False

convert8 :: Quantum -> Bool
convert8 Yes  = False
convert8 No   = False 
convert8 Both = False 

