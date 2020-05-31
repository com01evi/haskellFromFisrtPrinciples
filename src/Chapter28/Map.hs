module Chapter28.Map(
  mapMain,
  setMain,
  seqMain,
  vectorMain
)where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Se
import qualified Data.Vector as V

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
