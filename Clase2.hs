min' :: Ord a => a -> a -> a
min' = \ x y -> if x < y then x else y

succ' :: Int -> Int
succ' = \ n -> n+1

div' :: Int -> Int -> Int
div' = div


mod' :: Int -> Int -> Int
mod' = \ a m -> a - a `div` m

-- Making a non-standard instance of Num.
instance Num a => Num (b -> a) where
    f + g = \ x -> f x + g x
    f * g = \ x -> f x * g x
    f - g = \ x -> f x - g x
    signum = id
    abs f = abs . f
    fromInteger n = \ x -> fromInteger n

multThree :: Num a => a -> a -> a -> a
multThree x y z = x * y * z

-- multThree 4 :: Num a => a -> a -> a

multThree' :: Num a => a -> a -> a -> a
multThree' x = \ y z -> x * y * z

applyTwice' :: (a -> a) -> (a -> a)
applyTwice' = \ f -> f . f

-- compare (-8) :: (Num a, Ord a) => a -> Ordering