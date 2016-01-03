module Data.Zlist
(
  Zlist,
  zlist,
  Data.Zlist.map,
  zmaximum
) where

import Control.Monad
import Control.Applicative

data Zlist a b = Zlist [(a, b)]

instance Functor (Zlist a) where
  fmap f (Zlist x) = Zlist $ fmap' f <$> x
    where
      fmap' :: (b -> c) -> (a, b) -> (a, c)
      fmap' f (a, b) = (a, f b)

instance (Show a, Show b) => Show (Zlist a b) where
  show (Zlist x) = join' (show' <$> x) "; "
    where show' (a, b) = show a ++ " => " ++ show b

zlist :: [a] -> Zlist a a
zlist x = Zlist $ zip x x

map :: (b -> c) -> Zlist a b -> Zlist a c
map = fmap

zmaximum :: (a -> a -> Bool) -> Zlist t a -> Zlist t a
zmaximum f (Zlist x) = Zlist [zmaximum' f x]
  where
    zmaximum' :: (a -> a -> Bool) -> [(t, a)] -> (t, a)
    zmaximum' f [] = error "could not get maximum of empty list"
    zmaximum' f [x] = x
    zmaximum' f [x@(_, xa), y@(_, ya)] = if f xa ya then x else y
    zmaximum' f (x:y:s) = zmaximum' f $ zmaximum' f [x, y] : s

join' :: [String] -> String -> String
join' [x] sep = x
join' (x:xs) sep = x ++ sep ++ join' xs sep
