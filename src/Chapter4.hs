module Chapter4
    (
      Mood(..)
    , changeMood
    , allAwesome
    , awesome
    , isPalindrome
    ) where

data Mood = Blah | Woot deriving(Show) 


changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome = (==) <$> id <*> reverse

isMaybeSame:: (Eq a) => a -> Maybe Bool
isMaybeSame a = (==) <$> (Just a) <*> (Just a)

myabs :: Integer -> Integer
myabs a
  | a < 0 = negate a
  | otherwise = a

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f t1 t2 = ((snd t1,snd t2), (fst t1, fst t2))
