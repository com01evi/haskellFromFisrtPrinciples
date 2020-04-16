module Chapter21.Traversable(
  mygroup
 ,mygroupOn
 ,grouplen
 ,lowercaseEachInterest
 ,mytraverse
 ,mysequenceA
 ,three
)where

import Data.Char(toLower)
import Chapter20.Foldable
import Chapter11(BTree(Leaf,Node))
import Morse

mytraverse2 :: (Functor t, Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
mytraverse2 f = sequenceA . fmap f

mygroup :: (Eq a) => [a] -> [[a]]
mygroup [] = []
mygroup (x:xs) = y : mygroup ys
  where y = filter (\n -> x == n) (x:xs)
        ys = filter (\n -> x /= n) xs

mygroupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
mygroupOn _ [] = []
mygroupOn f (x:xs) = y : mygroupOn f ys
  where y = filter (\n -> f x == f n) (x:xs)
        ys = filter (\n -> f x /= f n) xs

grouplen :: (a -> b) -> [[a]] -> [(b,Int)]
grouplen f = fmap ((,) <$> f . head <*> length)

lowercaseEachInterest :: [(Int, String)] -> [(Int, String)]
lowercaseEachInterest = fmap ((,) <$> fst <*> fmap toLower . snd)

class (Functor t, Foldable t) => MyTraverse t where
  mytraverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
  mytraverse = (mysequenceA .) . fmap
  mysequenceA :: (Applicative f) => t (f a) -> f (t a)
  mysequenceA = mytraverse id

instance MyTraverse [] where
  mytraverse _ [] = pure []
  mytraverse f (x:xs) = (:) <$> f x <*> mytraverse f xs

instance MyTraverse BTree where
  mytraverse _ Leaf = pure Leaf
  mytraverse f (Node left x right) = Node <$> (mytraverse f left) <*> (f x) <*> (mytraverse f right)

mymapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mymapM _ [] = return []
mymapM f (x:xs) = f x >>= (\y -> mymapM f xs >>= (\ys -> return (y:ys)))

mysequence :: (Monad m) => [m a] -> m [a]
mysequence = mymapM id

three :: Int -> Int -> Int -> Int
three x y z = z

data Query = Query

data SomeObj = SomeObj

data IoOnlyObj = IoOnlyObj

data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj,IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case mapM decodeFn a of
    (Left err) -> return $ Left err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

pipelineFn2 :: Query -> IO (Either Err [(SomeObj,IoOnlyObj)])
pipelineFn2 = (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn
