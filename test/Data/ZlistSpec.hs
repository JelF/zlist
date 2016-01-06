{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-binds #-}

module Data.ZlistSpec (spec) where

import Test.Hspec
import Data.Zlist
import Test.QuickCheck
import Control.Applicative
import Control.Monad
import Text.Show.Functions ()

instance (CoArbitrary t, Arbitrary t, Arbitrary a) => Arbitrary (Zlist t a) where
  arbitrary = liftM2 (\e f -> f <$> zlist e) (listOf arbitrary) arbitrary

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ZList data type" $
    describe "Functor instance" $
      describe "fmap" $ do
        it "obeys 'id' law" $ property $
          \ x -> fmap id x == (x :: Zlist Int Int)

        it "obeys associative law" $ property $
          \ x p q ->
            let _ = x :: Zlist Int Int
                _ = p :: String -> Rational
                _ = q :: Int -> String
            in fmap (p . q) x == (fmap p . fmap q) x

  describe "Show instance" $
    describe "show" $ do
      it "works" $ do
        show (zlist [1::Integer]) `shouldBe` "1 => 1"
        show (zlist ["1", "2"]) `shouldBe` "\"1\" => \"1\"; \"2\" => \"2\""
        show (show <$> zlist [1::Integer]) `shouldBe` "1 => \"1\""
      it "can show empty list" $
        let list = zlist ([]::[Integer])
        in show list `shouldBe` ""

  describe "zmaximum" $ do
    context "empty list" $
      it "returns empty zlist" $
        zmaximum compare (zlist ([]::[Int])) `shouldBe` zlist []
    context "one element" $
      it "returns only element" $
        zmaximum compare (zlist ([1]::[Int])) `shouldBe` zlist [1]
    context "two equal elements" $
      it "returns both" $
        zmaximum compare (zlist ([1, 1]::[Int])) `shouldBe` zlist [1, 1]
    context "two different elements" $ do
      context "comparator is `compare`" $
        it "returns biggest" $
          zmaximum compare (zlist ([1, 2]::[Int])) `shouldBe` zlist [2]
      context "comparator is `flip compare`" $
        it "returns lowest" $
          zmaximum (flip compare) (zlist ([1, 2]::[Int])) `shouldBe` zlist [1]
    context "an array of elements" $
      it "return biggest" $
        let list = zlist $ [0,2..10] ++ [9, 3, -2] ++ [-1, 1 .. 7] :: Zlist Int Int
        in zmaximum compare list `shouldBe` zlist [10]
