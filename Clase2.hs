min' :: Ord a => a -> a -> a
min' = \ x y -> if x < y then x else y

succ' :: Int -> Int
succ' = \ n -> n+1

div' :: Int -> Int -> Int
div' = div


mod' :: Int -> Int -> Int
mod' = \ a m -> a - a `div` m