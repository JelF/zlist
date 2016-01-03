{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-binds #-}
{-# LANGUAGE InstanceSigs #-}

module Data.ZlistSpec (spec) where

import Test.Hspec
import Data.Zlist
import Test.QuickCheck
import Control.Applicative
import Control.Monad
import Text.Show.Functions ()

instance (Arbitrary t, Arbitrary a) => Arbitrary (Zlist t a) where
  arbitrary :: Gen (Zlist t a)
  arbitrary = liftM2 arbitrary' (listOf arbitrary) arbitrary
    where
      arbitrary' :: [t] -> a -> Zlist t a
      arbitrary' x y = const y <$> zlist x

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ZList data type" $
    describe "is a functor" $
      describe "fmap" $ do
        it "obeys 'id' law" $ property $
          \ x -> fmap id x == (x :: Zlist Int Int)

        it "obeys associative law" $ property $
          \ x p q ->
            let _ = x :: Zlist Int Int
                _ = p :: String -> Bool
                _ = q :: Int -> String
            in fmap (p . q) x == (fmap p . fmap q) x

  describe "show" $
    it "works" $ do
      show (zlist [1::Integer]) `shouldBe` "1 => 1"
      show (zlist ["1", "2"]) `shouldBe` "\"1\" => \"1\"; \"2\" => \"2\""
      show (show <$> zlist [1::Integer]) `shouldBe` "1 => \"1\""
