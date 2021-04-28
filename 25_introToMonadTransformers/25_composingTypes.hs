{-p LANGUAGE InstanceSigs #-}
module ComposingTypes where

import Control.Applicative (liftA2, liftA3)


-- | Monad Transformers
--
--  Chapters 25 & 26 will be focusing on monad transformers, in both 
--  principles and practicality.
--
--
-- | Composing types
-- 
--  One recalls that functors and applicatives are both closed under 
--  composition i.e. the composition of two functors (or applicatives) 
--  returns another functor (or applicative). This is not necessarily 
--  true of monads.

--  However, there are occassions where such behavior is desired from
--  monads. Due to the fact that monads allow us to work with different
--  effects, composing monads allows us to build up computations with
--  multiple effects. By stacking a `Maybe` monad with an `IO` monad, we
--  can perform IO actions while also building up computations that have
--  a possibility of failure, handled by the `Maybe` Monad.

--  A monad transformer thus, is a variant of an ordinary type that
--  takes an additional type argument that is assumed to have a Monad
--  instance.  The transformer variant of a type gives us a Monad
--  instance that binds over bits of structure, allowing us to compose
--  monads and combine their effects. For instance, `MaybeT` is the
--  transformer variant of the `Maybe` type.

--  The aims of this chapter are as follows: 
--    1. Demonstrate why composing 2 monads does not necessarily return
--       a monad 
--    2. Examine the `Identity` and `Compose` types 
--    3. Manipulate types until we can make monads compose.  
--    4. Introduce some common monad transformers 
--    5. Work through an example



-- | Common functions as types
--
--  We start off with defining some =newtypes= that correspond to basic
--  functions. The idea here is to use these datatypes as helpers to
--  demonstrate problems with composing monads, before we exhibit how
--  monad transformers help.


--  Identity as a type

newtype Identity a =
  Identity { runIdentity :: a } deriving (Eq,Show)

--  NOTE: We could have written the above with a =data= keyword, but
--  convention around monad transformers favors =newtype= heavily. Not
--  only do newtypes have a representation identical to the type they
--  contain, the keypoint that monad transformers are a means of
--  wrapping a monadic layer of structure aroung another type naturally
--  lends itself to newtypes.

--  The kind of the newtype =Identity= we defined is similar to the type
--  of the =id= function, reflecting the nature of lifting of the type
--  enclosed by =Identity= to a higher monadic layer.

--    id :: a -> a
--    Identity :: * -> *


--  Composition as a type

newtype Compose f g a = 
  Compose { getCompose :: f (g a) } deriving (Eq, Show)

--  In the case of =Compose=, =f= and =g= must be type constructors
--  themselves, with =a= as the concrete type. To illustrate this,
--  consider =Compose [Just (1 :: Int), Nothing]=

--    with xs = [Just 1, Nothing] :: [Maybe Int],
--      Compose { getCompose = xs } :: Compose [] Maybe Int

-- Similar to the =Identity= newtype, the =Compose= newtype has a kind
-- that resembles its function counterpart =.=:

--    (.) :: (b -> c) -> (a -> b) -> a -> c
--    Compose :: (* -> *) -> (* -> *) -> * -> *


--  Functors for Monads

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

--  The functor of Identity does not do anything particularly
--  interesting, and serves only to illustrate the essence of what a
--  Functor is meant to be. The function gets lifted into the context of
--  the Identity type & subsequently mapped over the `a` value.

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

--  Both `f` and `g` are part of the structure that we're lifting over,
--  and naturally have to be Functor instances themselves. And since
--  we're lifting over both the `f` and `g` structures, we have to apply
--  `fmap` twice.

--    with xs = [Just 1, Nothing],
--      (+1) <$> (Compose xs) == Compose { getCompose = [ Just 2
--                                                      , Nothing] }

--  With Compose, we now have the ability to express arbitrarily nested
--  types:

--    foo :: Compose [] Maybe (Compose Maybe [] Integer)
--    foo = Compose [Just (Compose $ Just [1])]

--  Compose exemplifies functor being "closed under composition", which 
--  means that composition of datatypes with Functor instances gives
--  rise to a new Functor instance.



-- | Twinplicative

--  As with Functors, Applicatives are also closed under composition.
--  Composing two datatypes with Applicative instances should return a
--  new Applicative instance. 

--  Exercise: Write the Applicative instance for Compose
instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (<*>) (Compose f) (Compose a) = Compose $ (<*>) <$> f <*> a


-- | Twonad

--  With monads, the pattern with seen thus far of being closed under
--  composition does not necessarily hold. While composing two arbitrary
--  datatypes with Monad instances does not break, the output is not
--  necessarily a monad.

--  Suppose we were to write a Monad instance for Compose
--
--    instance (Monad f, Monad g) =>
--             Monad (Compose f g) where
--      return = pure
--
--      (>>=) :: Compose f g a 
--            -> (a -> Compose f g b)
--            -> Compose f g b

--  If we study the bind operation, we see that because `f` and `g` have
--  their own Monad instances (and their own implementations of bind
--  necessarily):

--    Monad f => f a -> (a -> f b) -> f b
--    Monad g -> g a -> (a -> g b) -> g b

--  The monadic bind of Compose would then have the type:

--    (Monad f, Monad g) =>
--      f (g a) -> (a -> f (g b)) -> f (g b)

--  or equivalently, (because we're able to define a monadic instance
--  with `join` instead of bind)

--    (Monad f, Monad g) =>
--      f (g (f (g a))) -> f (g a)

--  As a general rule, this is not possible for all monadic instances.
--  ( NOTE: See https://stackoverflow.com/a/55046691 or
--  https://stackoverflow.com/a/29454112 for a description of why not)


--  Exercise: Compose instances
--  
--  1) Write the Compose Foldable instance
instance (Foldable f, Foldable g) =>
  Foldable (Compose f g) where
    foldMap :: (Monoid m) => (a -> m) -> Compose f g a -> m
    foldMap h (Compose fga) = (foldMap . foldMap) h fga  
-- Compose { getCompose :: f (g a) }
-- foldMap :: (Monoid m) => (a -> m) -> Compose f g a -> m
-- (Foldable f) => (Monoid m') => (m'' -> m') -> t m'' -> m'
-- (Foldable g) => (Monoid m'') => (a -> m'') -> t a -> m''

--  2) Write the Traversable instance for Compose
instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
    traverse :: (Applicative h, Traversable f, Traversable g) =>
      (a -> h b) -> Compose f g a -> h (Compose f g b)
    traverse h (Compose fga) = Compose <$> (traverse . traverse) h fga


--  3) Write instances of the following "Bifunctor" typeclass for the
--  various datatypes

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c 
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 3.1
data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

-- 3.2
newtype Const a b = Const a deriving Show

instance Bifunctor Const where
  bimap f g (Const a) = Const (f a)

-- 3.3
data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 3.4
data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

-- 3.5
data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a  

-- 3.6
data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

-- 3.7
data BeepBoop a b = Beep a | Boop b

instance Bifunctor BeepBoop where
  bimap f _ (Beep a) = Beep $ f a
  bimap _ g (Boop b) = Boop $ g b



-- | Monad Transformers
--  
--  We've previously mentioned that the issue with `join`-ing two
--  arbitrary monads was that they might not necessarily commute. To
--  make that `join` possible, we need to reduce the polymorphism and
--  get concrete information about one of the monads involved, while the
--  other monad remains polymorphic as a variable type argument to the
--  type constructor. 
--
--  Transformers thus allow us to make a monad out of multiple different
--  types that each have a Monad instance by wrapping these existing
--  monads. In this section we will walk through the process of writing
--  monad transformers.


--  Monadic stacking
--  
--  Applicative allows us to apply functions of more than one argument
--  (provided arguments have functorial structure), enabling us to do
--  things like:
--
--    (,,) <$> Just 1 <*> Just "lol" <*> Just [1,2]
--
--  We want to try and achieve the same thing with monads. A particular
--  case that will likely be seen commonly is with IO & Reader. IO
--  performs effectful actions like talking to a database, while Reader
--  would handle the database connection and/or the HTTP request
--  context. 
--
--
--  IdentityT
newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving Show

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  -- remember that the IdentityT data-constructor is wrapping around
  -- something that already has some level of structure, and is
  -- providing "Identity"-like functionality via the wrapper
  --
  -- Thus the fab & fa arguments
  (<*>) (IdentityT fab) (IdentityT fa) = IdentityT $ fab <*> fa 


instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (>>=) (IdentityT ma) f = IdentityT $ ma >>= runIdentityT . f

--  Breaking down the bind operation for IdentityT, recall that the type
--  signature of the bind (concretized to this case) is as follows:
--    
--    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
--
--  with f :: (a -> IdentityT m b),
--
--    runIdentityT . f :: a
--
--  the concrete value of the `a`-typed value in the above is provided
--  from the (>>=) prior. This bind operation within the definition is
--  provided by the (Monad m) constraint, and relates specifically to
--  the `ma` value. 

--  With the above, we can now bind over IdentityT like so 
--
--    Prelude> sumR = return . (+1)
--    Prelude> x = IdentityT [1, 2, 3] >>= sumR
--    IdentityT {runIdentityT = [2,3,4]}
--    Prelude> runIdentityT x
--    [2,3,4]
--
--  The IdentityT structure allows us to retain the "Identity"
--  functionality!
