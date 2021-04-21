{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module MonadTransformers where


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
