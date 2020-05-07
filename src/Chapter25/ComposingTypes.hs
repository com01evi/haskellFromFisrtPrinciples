{-# LANGUAGE InstanceSigs #-}

module Chapter25.ComposingTypes(
  Identity(Identity,runIdentity),
  Compose(Compose, getCompose)  
)where

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
