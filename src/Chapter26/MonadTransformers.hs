{-# LANGUAGE FlexibleContexts #-}

module Chapter26.MonadTransformers(
repeat',
loan,
result,
StateT(StateT,runStateT),
ReaderT(ReaderT, runReaderT),
game,
loop,
rPrintAndInc,
sPrintIncAccum
)where

import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad

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
                                     runStateT (f a) s'


instance MonadTrans (EitherT e) where
  lift = EitherT . (liftM Right)

instance MonadTrans (StateT s) where
  lift m = StateT f
           where f s = do
                       a <- m
                       return (a, s)



newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT mma) = MaybeT ((fmap . fmap) f mma)

instance Applicative m => Applicative (MaybeT m) where
  pure x = (MaybeT . pure . Just) x
  (MaybeT mmf) <*> (MaybeT mma) = MaybeT ((<*>) <$> mmf <*> mma)

instance Monad m => Monad (MaybeT m) where
  (MaybeT mma) >>= f = MaybeT (mma >>= (\ma -> case ma of
                                                 Just a -> (runMaybeT . f) a
                                                 Nothing -> return Nothing))

instance MonadTrans (MaybeT) where
  lift = MaybeT . (liftM Just)

instance (MonadIO m) => MonadIO(MaybeT m) where
  liftIO = lift . liftIO

instance MonadTrans (ReaderT r) where
  lift m = ReaderT (const m)

instance (MonadIO m) => MonadIO(ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO(StateT s m) where
  liftIO = lift . liftIO

type Date = Int
type Rate = Float 
type Amount = Float
type Loan = Float

loan :: Rate -> Amount -> Loan
loan r a = a + (a * r)

repeat' :: (a -> a) -> Int -> a -> a
repeat' f 0 x = x
repeat' f n x = repeat' f (n-1) (f x)

result :: Float -> Int -> Float
result amount count = foldr (\f acc -> f acc) amount $ replicate count (loan 0.1)

game :: StateT Int IO Int
game = StateT f
       where f n = do
                   x <-  (read <$> getLine :: IO Int)
                   y <-  (read <$> getLine :: IO Int)
                   putStrLn $ "C is: " ++ show x
                   putStrLn $ "You are: " ++ show y
                   if x == y 
                   then do 
                          putStrLn "You win!"
                          print $ (0, n+1)
                          return (0, n+1)
                   else do
                          putStrLn "You lose"
                          print $ (0, n)
                          return (0, n)

loop :: StateT Int IO Int -> StateT Int IO Int
loop g = do
         g
         state <- get'
         if state == 3
         then g
         else loop g

get' :: (Monad m) => StateT s m s
get' = StateT f 
       where f s = do
                   return (s, s)
 
type Reader r a = ReaderT r Identity a

rDec :: Num a => Reader a a
rDec = ReaderT (Identity . (subtract 1))

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT (Identity . show)

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT f
               where f n = do
                           putStrLn $ "Hi: " ++ show n
                           return (n+1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT f
                 where f n = do
                             putStrLn $ "Hi: " ++ show n
                             return (show n, n+1)

isValid :: String -> Bool
isValid v = elem '!' v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
  v <- getLine
  guard $ isValid v
  return (Just v)

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite 
    case excite of
      Nothing -> putStrLn "MOAR EXCITE"
      Just e -> putStrLn $ "Good, was very excite: " ++ e
