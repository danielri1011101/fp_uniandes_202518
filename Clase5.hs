-- Reimplementing Functor ideas

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f = \ mx -> case mx of
    Just x -> Just $ f x
    Nothing -> Nothing

class Functor f => Aplicative1 f where
    pure1 :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b

-- Usual lists define a functor by the standard prelude 
instance Aplicative1 [] where
    pure1 = \ x -> [x]
    fs <**> xs = [f x | f <- fs, x <- xs]

newtype ZipList1 a = ZipList1 {getIt :: [a]}

instance Functor ZipList1 where
    fmap f = ZipList1 . fmap f . getIt

instance Aplicative1 ZipList1 where
    pure1 = ZipList1 . repeat
    zl_fs <**> zl_xs = ZipList1 $ zipWith ($) (getIt zl_fs) (getIt zl_xs)