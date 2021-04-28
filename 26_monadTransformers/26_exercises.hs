{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module MonadTransformers where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Control.Monad (liftM)

import Test.Hspec

-- | Monad Transformers (Round 2)
--
--  Chapter objectives:
--    - Work through & showcase more monad transformer types and
--      instances
--    - Study how ordering and wrapping of monad transformer stacks
--      affects their performance
--    - Solidify conceptual knowledge through exercises


-- | MaybeT
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure a = MaybeT $ pure . pure $ a
  (<*>) (MaybeT mab) (MaybeT ma) = MaybeT $ (<*>) <$> mab <*> ma

-- A note about what's happening with the (<*>) definition: (here we are
-- neglecting to write the MaybeT wrapper)
--
--  (<$>) :: Functor f => (a -> b) -> f a -> f b
--  (<*>) :: Applicative f => f (a -> b) -> f a -> f b
--  mab   :: Applicative m => m (Maybe (a -> b))
--
--  (<*>) <$>     :: Applicative m, Functor f 
--                => f (m (a -> b)) -> f (m a -> m b) 
--  (<*>) <$> mab :: Applicative m => m (Maybe a -> Maybe b)
--
--  ma :: Applicative m => m (Maybe a)

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f = MaybeT $ ma >>= \case
                                         Just x -> runMaybeT $ f x
                                         Nothing -> return Nothing



-- | EitherT
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) } 

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT meab) <*> (EitherT mea) = EitherT $ (<*>) <$> meab <*> mea 

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= f = EitherT $ mea >>= \case 
                                            Right x -> runEitherT . f $ x
                                            Left y  -> return (Left y)
-- f :: a -> EitherT e m b

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e 
swapEitherT (EitherT ema) = EitherT . fmap swapEither $ ema
  where swapEither :: Either e a -> Either a e
        swapEither (Left x) = Right x
        swapEither (Right y) = Left y

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f1 f2 (EitherT mab) = mab >>= \case
                                        Left a  -> f1 a
                                        Right b -> f2 b


-- | StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f m = StateT $ \s -> fmap (\(a,s') -> (f a, s')) $ runStateT m s  

-- Breaking this down:
--
--  - runStateT returns a function of type s -> m (a,s). naturally, the
--    StateT data constructor needs to take the same typed argument,
--    which explains the lambda function.

--  - The function wrapped by StateT returns a `m (a, s)` type. The
--    functorial constraint on m allows us to lift over the `m`, which
--    leaves only the `(a,s)` typed return value. We address this by
--    once again using a lambda function to apply the function to be
--    applied, `f`, on the `a` value.  

--  - Lastly, we need an input value for whatever we've already written.
--    That comes in the form of `runStateT m s`, where s is referring to
--    the `s` contained within `\s` in the initial lambda function, and 
--    `m` contains the "previous context" to be modified.


instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  (<*>) (StateT mabs) (StateT mas) = 
    StateT $ \s -> do
      x <- mabs s
      fmap (\(a,s') -> (fst x $ a, s')) $ mas s

--  - Notice the `Monad m` constraint on the Applicative instance.
--    It is used in place of an `Applicative m` constraint because
--    Applicative would result in the feeding of initial state, rather
--    than threading the value through operations.


instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do 
    -- sma s :: m (a, s) 
    x <- sma s
    -- x :: (a, s)
    -- fst x :: a
    -- f :: a -> StateT s m b
    -- f $ fst x :: StateT s m b
    runStateT (f . fst $ x) (snd x) 
    -- runStateT (f . fst $ x) :: s -> m (b, s)



-- | Exercise: Wrap it up
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT (pure . const (Right (Just 1)))



-- | Monad Trans
--  
--  With the MonadTrans typeclass, the idea is about lifting actions in
--  some Monad over a transformer type that wraps itself in the original
--  Monad. 

class MonadTrans t where
  -- Lifts a computation from the argument monad
  -- to the constructed monad
  lift :: (Monad m) => m a -> t m a 

--  As with type classes, the strength of MonadTrans is more apparent
--  when concretized to a specific datatype.
--
instance MonadTrans MaybeT  where
  lift :: (Monad m) => m a -> MaybeT m a
  lift = MaybeT . liftM Just 

--  The `lift` of MonadTrans takes an `m a` and lifts it into the
--  context of another concrete monad (that we know we want). In this
--  case, it has lifted (or transformed) it into a MaybeT context.  


-- | Exercises: Lift more
instance MonadTrans (EitherT e) where
  lift :: (Monad m) => m a -> EitherT e m a 
  lift = EitherT . liftM Right 

instance MonadTrans (StateT s) where
  lift :: (Monad m) => m a -> StateT s m a
  lift ma = StateT $ \s -> do
    a <- ma
    return (a,s)



--  MonadIO
--
--  MonadIO is a specialized case of a MonadTrans typeclass, and was
--  designed specifically to lift an IO action over all structure. 

class (Monad m) => MonadIO m where
  -- | Lift a computation from the 'IO' monad
  liftIO :: IO a -> m a 
  -- the IO action should now be "enclosed" within the m monad

--  Instances of MonadIO should satisfy the following laws which state
--  that liftIO is a transformer of monads
--
--    1) liftIO . return = return
--    2) liftIO (m >>= f) = liftIO m >>= (liftIO . f)

-- Exercises: some instances
instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO = MaybeT . liftM Just . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO x = ReaderT $ \r -> liftIO x  

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a 
  liftIO x = StateT $ \s -> do
    a <- liftIO x
    return (a,s)

instance MonadIO IO where
  liftIO :: IO a -> IO a
  liftIO x = x





-- | Chapter Exercises
--
--  Write the code

--  1) & 2)
rDec :: Num a => Reader a a
rDec = reader (-1 +)

--  3) & 4)
rShow :: Show a => ReaderT a Identity String
rShow = show <$> ask

-- 5)
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  val <- ask
  liftIO $ putStrLn ("Hi: " ++ show val) -- ReaderT a IO () 
  return (val + 1)

-- 6) 
get :: Monad m => StateT s m s
get = StateT $ \s -> pure (s,s)

put :: Monad m => s -> StateT s m ()
put x = StateT $ \_ -> pure ((), x)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT (\s -> 
  putStrLn ("Hi: " ++ show s) >> pure (show s, s+1))  



tests :: IO ()
tests = hspec $ do
  describe "rDec" $ do
    it "decrements by 1 given num-class value" $ do
      runReader rDec 1 `shouldBe` 0
    it "decrements all elements by 1 when fmapped to a list of num-class values" $ do
      fmap (runReader rDec) [1..10] `shouldBe` [0..9]
  describe "rShow" $ do
    it "shows \"1\" given 1" $ do
      runIdentity (runReaderT rShow 1)  `shouldBe` "1"
  describe "rPrintAndInc" $ do
    it "returns 2 given 1" $ do 
      a <- runReaderT rPrintAndInc 1
      a `shouldBe` 2
