pow :: Num a => a -> Int -> a
pow = \ x n -> powIter x n 1
  where
    powIter _ 0 accum = accum
    powIter x n accum
      | n > 0 = powIter x (n-1) (x*accum)
      | otherwise = error "No negative powers"

fooSum :: (Num a, Num b, Num c) => a -> b -> c
fooSum x y = -1

myLen :: [a] -> Int
myLen xs = iterLen xs 0
  where
    iterLen [] accum = accum
    iterLen (x:xs) accum = iterLen xs (1+accum)