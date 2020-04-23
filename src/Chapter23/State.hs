module Chapter23.State(
  MyState(MyState, runStae),
  state
)where

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

state :: MyState [Int] (Maybe Int)
state = do
  push 3
  push 4
  push 5
  pop
  push 2
  pop
