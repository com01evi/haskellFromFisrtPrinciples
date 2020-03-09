module Main where

import Chapter2(square)
import Chapter8(digitToWord, digits, wordNumber)
import Chapter11 (capitalizeWord)
import Chapter14.Exercise(half, halfIndentity, listOrdered, plusAssociative, multAssociative)
import Data.List(sort)
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec $ do 
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"

    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]

    it "returns [1,0,0] for 100" $ do
      digits 100 `shouldBe` [1,0,0]

  describe "wordNumber" $ do
    it "returns one-zero-zero for 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"

    it "returns nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

    it "returns value dividedBy 2" $ do
      property $ \x -> half x == (x / 2 :: Double)

    it "returns value dividedBy 2" $ do
      property $ \x -> half x == (x / 2 :: Float)

    it "returns identity value" $ do
      property $ \x -> halfIndentity x == x

    it "returns sorted list" $ do
      property $ \x -> (listOrdered . sort) (x :: [Int])

    it "returns sorted list" $ do
      property $ \x -> (listOrdered . sort) (x :: [Char])

    it "tests plus associative" $ do
      property $ \x y z-> plusAssociative (x :: Int) y z

    it "tests mult associative" $ do
      property $ \x y z-> multAssociative (x :: Int) y z

    it "tests law lelated to relationship of quot and rem" $ do
      property $ \x y -> (quot (x :: Int) y)*y + (rem x y) == x

    it "tests law lelated to relationship of quot and rem" $ do
      property $ \x y -> (div (x :: Int) y)*y + (mod x y) == x

    it "tests whether ^ is associative or not" $ do
      property $ \x y z-> (x :: Int) ^ ((y :: Int) ^ (z :: Int)) == (x ^ y) ^ z

    it "tests reverse of reverse is equivalent to id" $ do
      property $ \x -> (reverse . reverse) x == id (x :: [Int])

    it "tests definition of ($)" $ do
      property $ \x -> id $ (x :: Int) == id x

    it "tests definition of (.)" $ do
      property $ \x -> (id . id) (x :: Int) == id (id x)

    it "tests foldr (:) is equivalent to (++)" $ do
      property $ \xs ys -> foldr (:) xs ys == xs ++ (ys :: [Int])

    it "tests foldr (++) [] is equivalent to (concat)" $ do
      property $ \xs -> foldr (++) [] xs == concat (xs :: [String])

    it "tests length (take n xs) is equivalent to n" $ do
      property $ \n xs -> length (take n (xs :: [Int])) == n 

    it "tests (read . show) is equivalent to id" $ do
      property $ \x -> (read . show) (x :: Int) == (id x)

    it "tests square x is equivalent to x * x" $ do
      property $ \x -> square x == x * (x :: Float)

    it "tests square sqrt x is equivalent to id x" $ do
      property $ \x -> (square . sqrt) x == (x :: Float)

    it "tests capitalizeWord" $ do
      property $ \x -> capitalizeWord x == (twice capitalizeWord) x && capitalizeWord x == (fourTimes capitalizeWord) x 

    it "tests sort" $ do
      property $ \x -> sort (x ::[Int]) == (twice sort) x && sort x == (fourTimes sort) x

twice f = f . f
fourTimes = twice . twice

data Fool = Fulse 
          | Frue deriving(Eq, Show)
          
genFool :: Gen Fool
genFool = oneof [ return Fulse
                , return Frue]

frequentFool :: Gen Fool
frequentFool = frequency [(2, return Fulse)
                         ,(1, return Frue)]
