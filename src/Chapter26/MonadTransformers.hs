module Chapter26.MonadTransformers(
)where

import Control.Monad.Identity

innerMost :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ (pure . Right) x
  (EitherT mef) <*> (EitherT mea) = EitherT $ (<*>) <$> mef <*> mea

instance Monad m => Monad (EitherT e m) where
  (EitherT mea) >>= f = EitherT $ do
    ea <- mea
    case ea of
      Left x -> return $ Left x
      Right y -> (runEitherT . f) y

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mea) = do
  ea <- mea
  either f g ea

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT (\_ -> pure x)
  (ReaderT rmf) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmf <*> rma

instance Monad m => Monad (ReaderT r m) where
  (ReaderT rma) >>= f = ReaderT rmb
                        where rmb r = do 
                                      a <- rma r
                                      let g = (runReaderT . f) a
                                      g r


newtype StateT s m a = StateT {runStateT :: (s -> m (a, s))}

instance Functor m => Functor (StateT s m) where
  fmap f (StateT smas) = StateT smbs
                         where smbs s = fmap ((,) <$> f . fst <*> snd) mas
                                        where mas = smas s

instance Monad m => Applicative (StateT s m) where
  pure x = StateT (\s -> pure (x, s))
  (StateT smfs) <*> (StateT smas) = StateT smbs
                                    where smbs s = do
                                                   (f, s') <- smfs s
                                                   (a, s'') <- smas s'
                                                   pure (f a, s'')

instance Monad m => Monad (StateT s m) where
  StateT smas >>= f = StateT smbs
                      where smbs s = do
                                     (a, s') <- smas s
                                     let g = f a
                                     runStateT g s'
