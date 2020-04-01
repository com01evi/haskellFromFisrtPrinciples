module Chapter17.BadMonoid (
  bmmain
 ,bmmain2
)where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Chapter17.Applicative

data Bull = Fools | Twoo deriving(Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo)
                        ]

instance Semigroup Bull where
  _ <> Twoo = Twoo
  Twoo <> _ = Twoo
  Fools <> Fools = Fools
  
instance Monoid Bull where
  mempty = Fools
  mappend _ Twoo = Twoo
  mappend Twoo _ = Twoo
  mappend Fools Fools = Fools

instance EqProp Bull where
  (=-=) = eq

bmmain :: IO ()
bmmain = quickBatch (monoid Twoo)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

bmmain2 :: IO ()
bmmain2 = do
  let trigger = Identity ((1 :: Int), '2', "WWW")
  quickBatch $ applicative trigger


