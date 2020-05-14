module Chapter25.ComposingTypes(
  Identity(Identity,runIdentity),
  Compose(Compose, getCompose)  
)where

import Data.Bifunctor

newtype Identity a = Identity {runIdentity :: a}

newtype Compose f g a = Compose {getCompose :: f (g a)}

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure x = Identity x
  (Identity f) <*> (Identity x) = Identity (f x)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ fmap (fmap f) fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure (pure x))
  (Compose fgh) <*> (Compose fga) = Compose $ (<*>) <$> fgh <*> fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = foldMap (foldMap f) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux x y) = Deux (f x) (g y)

data Const a b = Const a

instance Bifunctor (Const) where
  bimap f _ (Const x) = Const (f x)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei x y z) = Drei x (f y) (g z)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei x y) = SuperDrei x (f y)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei x) = SemiDrei x

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data BiEither a b = BiLeft a | BiRight b

instance Bifunctor BiEither where
  bimap f _ (BiLeft x) = BiLeft (f x)
  bimap _ g (BiRight y) = BiRight (g y)

newtype IdentityT f a = IdentityT {runIdentityT :: f a} deriving(Eq, Show)
    
instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative f => Applicative (IdentityT f) where
  pure x = IdentityT (pure x)
  (IdentityT ff) <*> (IdentityT fa) = IdentityT (ff <*> fa)

instance Monad m => Monad (IdentityT m) where
  (IdentityT ma) >>= f = IdentityT (ma >>= runIdentityT . f)

 
