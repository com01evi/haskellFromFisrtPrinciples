module Chapter23.State(
  MyState(MyState, runStae),
  stackstate,
  randomState,
  nDie,
  rollsTogetTwenty,
  rollsCountLogged,
)where

import System.Random
import Control.Monad(replicateM)
import Control.Monad.Trans.State

newtype MyState s a = MyState {runStae :: s -> (a, s)}

instance Functor (MyState s) where
  fmap f (MyState g) = MyState h
                       where h s = (f a, s')
                                   where (a, s') = g s

instance Applicative (MyState s) where
  pure x = MyState (\s -> (x, s))
  (MyState f) <*> (MyState g) = MyState h
                                where h s = (f' x, s'')
                                            where (x, s'') = g s'
                                                  (f', s') = f s

instance Monad (MyState s) where
  MyState f >>= g = MyState h
                    where h s = runStae (g a) s'
                                where (a, s') = f s

push :: a -> MyState [a] (Maybe a)
push x = MyState (\s -> (Nothing, (x:s)))

pop :: MyState [a] (Maybe a)
pop = MyState (\(x:xs) -> (Just x, xs))

stackstate :: MyState [Int] (Maybe Int)
stackstate = do
  push 3
  push 4
  push 5
  pop
  push 2
  pop

randomState :: MyState StdGen Integer
randomState = do
  n <- MyState (random :: StdGen -> (Integer, StdGen))
  return n

data Die = DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix
         deriving(Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  x -> error $ "number must be under six: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do 
  (n, s) <- (randomR (1, 6))
  return (intToDie n, s)

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = (,,) <$> rollDie <*> rollDie <*> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsTogetTwenty :: StdGen -> Int
rollsTogetTwenty g = go 0 0 g
                     where go :: Int -> Int -> StdGen -> Int
                           go sum count gen
                             | sum >= 20 = count
                             | otherwise = let (die, nextGen) = randomR (1, 6) gen
                                           in go (sum + die) (count + 1) nextGen

rollsTogetN :: Int -> StdGen -> Int
rollsTogetN n g = go 0 0 g
                     where go :: Int -> Int -> StdGen -> Int
                           go sum count gen
                             | sum >= n = count
                             | otherwise = let (die, nextGen) = randomR (1, 6) gen
                                           in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int,[Die])
rollsCountLogged n g = go 0 (0, []) g
                     where go :: Int -> (Int, [Die]) -> StdGen -> (Int,[Die])
                           go sum (count,dielist) gen
                             | sum >= n = (count,dielist)
                             | otherwise = let (die, nextGen) = randomR (1, 6) gen
                                           in go (sum + die) ((count + 1, intToDie die: dielist)) nextGen
