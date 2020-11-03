module MemExercise where

newtype Mem s a = 
  Mem {runMem :: s -> (a,s)}

instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) = Mem $ (\s -> let (a,s') = f s    -- This one was a mindf. I knew that I
                                          (a',s'') = g s' -- had to use the tuple output somehow,
                                      in (a <> a', s''))  -- but couldn't figure out how to carry
                                                          -- the information in the a parameter 
                                                          -- forward. referred to johsi's ans

instance Monoid a => Monoid (Mem s a) where               
  mempty = Mem $ (\s -> (mempty, s)) 


-- program
f' = Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

tf1 = \s -> ("hi", s + 1)
tf2 = \s -> ("bye", s - 1)


