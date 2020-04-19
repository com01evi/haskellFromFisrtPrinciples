module Chapter21.Traversable(
  mygroup
 ,mygroupOn
 ,grouplen
 ,lowercaseEachInterest
 ,mytraverse
 ,mysequenceA
 ,three
 ,edgeMap
 ,foldMap'
 ,identityMain2
 ,constantMain2
 ,interests
 ,maybeMain2
 ,listMain2
 ,threeMain2
 ,pairMain2
 ,bigMain
 ,biggerMain
 ,sMain
 ,treeMain
)where

import Data.Char(toLower)
import Chapter20.Foldable
import Chapter11(BTree(Leaf,Node))
import qualified Chapter16.Functor as F
import Morse
import Data.Functor.Identity
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Chapter17.Applicative(ListA(NilA,ConsA))


mytraverse2 :: (Functor t, Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
mytraverse2 f = sequenceA . fmap f

interests=[(0,"Hadoop"),(0,"Big Data"),(0,"HBase"),(0,"Java"),(0,"Spark"),(0,"Storm"),(0,"Cassandra"),(1,"NoSQL"),(1,"MongoDB"),(1,"Cassandra"),(1,"HBase"),(1,"Postgres"),(2,"Python"),(2,"scikit-learn"),(2,"scipy"),(2,"numpy"),(2,"statsmodels"),(2,"pandas"),(3,"R"),(3,"Python"),(3,"statistics"),(3,"regression"),(3,"probability"),(4,"machine learning"),(4,"regression"),(4,"decision trees"),(4,"libsvm"),(5,"Python"),(5,"R"),(5,"Java"),(5,"C++"),(5,"Haskell"),(5,"programming languages"),(6,"statistics"),(6,"probability"),(6,"mathematics"),(6,"theory"),(7,"machine learning"),(7,"scikit-learn"),(7,"Mahout"),(7,"neural networks"),(8,"neural networks"),(8,"deep learning"),(8,"Big Data"),(8,"artificial intelligence"),(9,"Hadoop"),(9,"Java"),(9,"MapReduce"),(9,"Big Data")]

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

edgeMap :: (Traversable f) => (a -> b) -> f a -> f b
edgeMap f = runIdentity . traverse (Identity . f)

foldMap' :: (Monoid m, Traversable t) => (a -> m) -> t a -> m
foldMap' f = getConst . traverse (Const . f)

instance (Eq a) => EqProp (F.Identity a) where
  (=-=) = eq

instance Traversable F.Identity where
  traverse f (F.Identity x) = F.Identity <$> f x

identityMain2 :: IO ()
identityMain2 = do
  let trigger :: F.Identity (Int, Char, String)
      trigger = undefined
  quickBatch $ traversable trigger

instance (Eq a) => EqProp (F.Constant a b) where
  (=-=) = eq

instance Traversable (F.Constant a) where
  traverse f (F.Constant x) = pure (F.Constant x)

constantMain2 :: IO ()
constantMain2 = do
  let trigger :: F.Constant Int (Int, Char, String)
      trigger = undefined
  quickBatch $ traversable trigger

instance (Eq a) => EqProp (F.MyMaybe a) where
  (=-=) = eq

instance Traversable (F.MyMaybe) where
  traverse _ MyNothing = pure MyNothing
  traverse f (MyJust x) = MyJust <$> f x

maybeMain2 :: IO ()
maybeMain2 = do
  let trigger :: F.MyMaybe (Int, Char, String)
      trigger = undefined
  quickBatch $ traversable trigger

instance Traversable ListA where
  traverse _ NilA = pure NilA
  traverse f (ConsA x list) = ConsA <$> f x <*> traverse f list

listMain2 :: IO ()
listMain2 = do
  let trigger :: ListA (Int, Char, String)
      trigger = undefined
  quickBatch $ traversable trigger

instance Traversable (F.Three a b) where
  traverse f (F.Three x y z) = F.Three <$> pure x <*> pure y <*> f z

threeMain2 :: IO ()
threeMain2 = do
  let trigger :: F.Three Int Int (Int, Char, String)
      trigger = undefined
  quickBatch $ traversable trigger

instance (Eq a, Eq b) => EqProp (F.Pair2 a b) where
  (=-=) = eq

instance Traversable (F.Pair2 a) where
  traverse f (F.Pair2 x y) = F.Pair2 <$> pure x <*> f y

pairMain2 :: IO ()
pairMain2 = do
  let trigger :: F.Pair2 Int (Int, Char, String)
      trigger = undefined
  quickBatch $ traversable trigger

instance (Eq a, Eq b) => EqProp (F.Big a b) where
  (=-=) = eq

instance Traversable (F.Big a) where
  traverse f (F.Big x y z) = F.Big <$> pure x <*> f y <*> f z

bigMain :: IO ()
bigMain = do
  let trigger :: F.Big Int (Int, Char, String)
      trigger = undefined
  quickBatch $ traversable trigger

instance (Eq a, Eq b) => EqProp (F.Bigger a b) where
  (=-=) = eq

instance Traversable (F.Bigger a) where
  traverse f (F.Bigger a b c d) = F.Bigger <$> pure a <*> f b <*> f c <*> f d

biggerMain :: IO ()
biggerMain = do
  let trigger :: F.Bigger Int (Int, Char, String)
      trigger = undefined
  quickBatch $ traversable trigger

--instance (Applicative n, Testable ( n Property), EqProp a) => EqProp (F.S n a) where
--  (F.S x y) =-= (F.S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance (Eq (n a), Eq a) => EqProp (F.S n a) where 
  (=-=) = eq

instance (Traversable n) => Traversable (F.S n) where
  traverse f (F.S fx x) = F.S <$> traverse f fx <*> f x

sMain :: IO ()
sMain = do
  let trigger :: F.S [] (Int, Char, String)
      trigger = undefined
  quickBatch $ traversable trigger 

instance (Eq a) => EqProp (BTree a) where 
  (=-=) = eq

instance Traversable BTree where
  traverse f Leaf = pure Leaf
  traverse f (Node left x right) = Node <$> traverse f left <*> f x <*> traverse f right

treeMain :: IO ()
treeMain = do
  let trigger :: BTree (Int, Char, String)
      trigger = undefined
  quickBatch $ traversable trigger
