module Data.Zlist
(
  Zlist,
  zlist,
  zlist2,
  Data.Zlist.map,
  zmaximum
) where

import Control.Monad ()
import Control.Applicative

data Zlist a b = Zlist [(a, b)] deriving (Eq)

instance Functor (Zlist a) where
  fmap f (Zlist x) = Zlist $ fmap f <$> x

instance (Show a, Show b) => Show (Zlist a b) where
  show (Zlist x) = join' (show' <$> x) "; "
    where show' (a, b) = show a ++ " => " ++ show b

zlist :: [a] -> Zlist a a
zlist x = Zlist $ zip x x

zlist2 :: (a -> t) -> (a -> b) -> [a] -> Zlist t b
zlist2 f g x = Zlist $ zip (f <$> x) (g <$> x)

map :: (b -> c) -> Zlist a b -> Zlist a c
map = fmap

zmaximum :: (a -> a -> Ordering) -> Zlist t a -> Zlist t a
zmaximum f (Zlist inner) = Zlist $ zmaximum' inner []
  where
    -- haskell is really mad about this, lol. IDK why the fuck a here is not
    --   bounded to a in `zmaximum`
    -- zmaximum' :: [(t, a)] -> [(t, a)] -> [(t, a)]
    zmaximum' [] y = y
    zmaximum' [x] [] = [x]
    zmaximum' [x@(_, xa)] ys@((_, ya):_) = case f xa ya of
                                              LT -> ys
                                              EQ -> x : ys
                                              GT -> [x]
    zmaximum' (x:xs) y = zmaximum' xs $ zmaximum' [x] y

join' :: [String] -> String -> String
join' [] _ = ""
join' [x] _ = x
join' (x:xs) sep = x ++ sep ++ join' xs sep
