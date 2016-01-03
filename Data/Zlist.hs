module Data.Zlist
(
  Zlist,
  zlist,
  Data.Zlist.map,
  zmaximum
) where

import Control.Monad
import Control.Applicative

data Zlist a b = Zlist [(a, b)] deriving (Eq)

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

zmaximum :: (a -> a -> Ordering) -> Zlist t a -> Zlist t a
zmaximum f (Zlist x) = Zlist $ zmaximum' x []
  where
    -- haskell is really mad about this, lol. IDK why the fuck a here is not
    --   bounded to a in `zmaximum`
    -- zmaximum' :: [(t, a)] -> [(t, a)] -> [(t, a)]
    zmaximum' [] x = x
    zmaximum' [x] [] = [x]
    zmaximum' [x@(_, xa)] ys@(y@(_, ya):_) = case f xa ya of
                                              LT -> ys
                                              EQ -> x : ys
                                              GT -> [x]
    zmaximum' (x:xs) y = zmaximum' xs $ zmaximum' [x] y

join' :: [String] -> String -> String
join' [x] sep = x
join' (x:xs) sep = x ++ sep ++ join' xs sep
