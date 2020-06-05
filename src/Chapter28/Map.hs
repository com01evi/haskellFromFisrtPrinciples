module Chapter28.Map(
  mapMain,
  setMain,
  seqMain,
  vectorMain,
  vecMapMain,
  batchMain,
  dlistMain,
  popState,
  Queue(Queue),
  pushState,
  popl,
  queueMain
)where

import Control.Monad.Trans.State
import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Se
import Data.Vector ((//))
import qualified Data.Vector as V
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM

genList :: Int -> [(String, Int)]
genList n = go n []
  where go 0 xs = ("0", 0) : xs
        go n' xs = go (n'-1) ((show n', n') : xs)

pairList :: [(String, Int)]
pairList = genList 9001

testMap :: M.Map String Int
testMap = M.fromList pairList

mapMain :: IO ()
mapMain = defaultMain [ bench "lookup one thing, list" $
                        whnf (lookup "doesntExist") pairList
                      , bench "lookup one thing, map" $
                        whnf (M.lookup "doesntExist") testMap]

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0,0)

m' :: M.Map Int Int
m' = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (10000,10000)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

s' :: S.Set Int
s' = S.fromList $ take 10000 stream
  where stream = iterate (+1) 10000

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

setMain :: IO ()
setMain = defaultMain [ bench "member check map" $
                        whnf membersMap 9999
                      , bench "member check set" $
                        whnf membersSet 9999
                      , bench "insert value to map" $
                        whnf (M.insert 10001 10001) m
                      , bench "insert value to set" $
                        whnf (S.insert 10001) s
                      , bench "union map" $
                        whnf (M.union m) m'
                      , bench "union set" $
                        whnf (S.union s) s'
                      ]

lists :: [[Int]]
lists = replicate 10 [1..100000]

seqs :: [Se.Seq Int]
seqs = replicate 10 $ Se.fromList [1..100000]

lists2 :: [Int]
lists2 = [1..100000]

seqs2 :: Se.Seq Int
seqs2 = Se.fromList [1..100000]

seqMain :: IO ()
seqMain = defaultMain [ bench "concatenate lists" $
                        nf mconcat lists
                      , bench "concatenate sequences" $
                        nf mconcat seqs
                      , bench "indexing lists" $
                        whnf (!! 9001) lists2
                      , bench "indexing sequence" $
                        whnf (flip Se.index 9001) seqs2
                      ]

slice :: Int -> Int -> [a] -> [a]
slice from len xs = take len $ drop from xs

l :: [Int]
l = [1..1000]

v :: V.Vector Int
v = V.fromList [1..1000]

vectorMain :: IO ()
vectorMain = defaultMain [ bench "slicing list" $
                           whnf (head . slice 100 900) l
                         , bench "slicing vector" $
                           whnf (V.head . V.slice 100 900) v
                         ] 

testV' :: Int -> V.Vector Int
testV' n = V.map (n+) $ V.map (n+) $ V.map (n+) $ V.map (n+) $ V.fromList [1..10000]

testV :: Int -> V.Vector Int
testV n = V.map ((n+) . (n+) . (n+) . (n+)) $ V.fromList [1..10000]

vecMapMain :: IO ()
vecMapMain = defaultMain [ bench "vector map prefused" $
                           whnf testV 9998
                         , bench "vector map will be fused" $ 
                           whnf testV' 9998
                         ]

vec :: V.Vector Int
vec = V.fromList [1..10000]

slow :: Int -> V.Vector Int
slow n = go n vec
         where go 0 v = v
               go n v = go (n-1) (v // [(n, 0)])

batchList :: Int -> V.Vector Int
batchList n = vec // updates
              where updates = fmap (\i -> (i, 0)) [0..n]

batchMain :: IO ()
batchMain = defaultMain [ bench "slow" $
                          whnf slow 9998
                        , bench "batch list" $
                          whnf batchList 9998]

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do 
  mvec <- GM.new (n+1)
  go n mvec 
    where go 0 v = return v
          go n v = (MV.write v n 0) >> go (n-1) v

mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n+1)
  go n mvec 
    where go 0 v = V.freeze v
          go n v = (MV.write v n 0) >> go (n-1) v

newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL id

singleton :: a -> DList a
singleton x = DL (\xs -> x:xs)

toList :: DList a -> [a]
toList dl = unDL dl []

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)

infixr `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL ( (++[x]) . unDL xs)

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

dlistMain :: IO ()
dlistMain = defaultMain [ bench "concat list" $
                          whnf schlemiel 123456
                        , bench "concat dlist" $
                          whnf constructDlist 123456]

data Queue a = Queue { enqueue :: [a]
                     , dequeue :: [a]}deriving(Eq,Show)

push :: a -> Queue a -> Maybe (a, Queue a)
push x (Queue xs ys) = Just (x, Queue (x:xs) ys)

pushState :: a -> StateT (Queue a) Maybe a
pushState x = StateT $ push x 

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue xs []) = pop (Queue [] (reverse xs))
pop (Queue xs (y:ys)) = Just (y, Queue xs ys)

popState :: StateT (Queue a) Maybe a
popState = StateT pop

push' :: a -> [a] -> Maybe (a, [a])
push' x xs = Just (x, (x:xs))

pushState' :: a -> StateT [a] Maybe a
pushState' x = StateT $ push' x

pop' :: [a] -> Maybe (a, [a])
pop' [] = Nothing
pop' xs = Just (x , xs')
          where (x:xs') = reverse xs

popState' :: StateT [a] Maybe a
popState' = StateT pop'

pushl = runStateT (sequence (fmap pushState' [1..10000])) []

popl = runStateT (sequence (replicate 9999 popState' ++ fmap pushState' [1..10000])) 
f = getList' . popl

getList' :: Maybe ([a], [a]) -> [a]
getList' Nothing = []
getList' (Just (_, xs)) = xs

popq = runStateT (sequence (replicate 9999 popState ++ fmap pushState [1..100000]))

getList :: Maybe ([a], Queue a) -> [a]
getList Nothing = []
getList (Just (_, q)) = dequeue q

g = getList . popq

queueMain :: IO ()
queueMain = defaultMain [ bench "list push and pop" $
                          whnf f []
                        , bench "queue push and pop" $
                          whnf g (Queue [] [])]
