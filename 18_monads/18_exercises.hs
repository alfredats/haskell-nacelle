module Chapter18 where

import Control.Monad

-- Monads!
--
--  We're finally here, at the topic that everyone writes articles about.
--
--  Typedef of Monad:
--    
--    class Applicative m => Monad m where 
--      (>>=)  :: m a -> (a -> m b) -> m b
--      (>>)   :: m a -> m b -> m b
--      return :: a -> m a

--  Monads are stronger than Applicatives, which is in turn stronger than
--  Functors. This allows us to derive Applicatives and Functors in terms
--  of Monad. Concretely, this means we can write fmap in terms of monadic
--  operations. 
--
--      'fmap f xs' is equivalent to 'xs >>= return . f'
--  
--  Looking at the type signatures of (>>=) and 'return', it's easy to 
--  see. 
--        
--      return :: Monad m => a -> m a
--      f :: a -> b
--      (return . f) :: Monad m => a -> m b
--
--  The type signature of (return . f) is exactly what (>>=) takes. And 
--  with (return . f) settled, the behavior of (>>=) is exactly the same
--  as (fmap f). 
--
--
--
--  CORE OPERATIONS OF MONADS
--
--  The minimally complete Monad instance only requires the (>>=) 
--  operation defined. 'return' is essentially 'pure' from Applicative,
--  while (>>) a.k.a. the sequencing operator, sequences two actions while
--  discarding the result of the first. Lastly, (>>=) is known as the 
--  bind operator.
--
--  The bind operator has a type signature that is similar to fmap and 
--  (<*>). How it differs, is that monadic bind takes a function that 
--  introduces structure i.e. (a -> f b), applies it to the input values
--  that have the same monadic structure (f a), and returns values that
--  have one layer of structure discarded (f b instead of f (f b)).
--
--  This is largely due to the existence of the monadic join operation,
--  which is the monadic implementation of concat.
--
--      join :: Monad m => m (m a) -> m a
--
--
--  In summary, monadic bind takes a function that alters the structure
--  of the original input, then flattens the two layers of structure into
--  one.


-- Exercise : The answer is the exercise. Write bind in terms of fmap
--            and join.

bind :: Monad m => (a -> m b) -> m a -> m b
bind f xs = join $ fmap f xs    











