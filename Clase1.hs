import Control.Monad.Accum (MonadAccum(accum))
import System.Win32 (COORD(xPos))
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

pullElem :: Num a => Int -> [a] -> a
pullElem n xs
  | n < 0 = -1
  | otherwise = iterElem (n+1) xs (-1)
    where
        iterElem 0 _ accum = accum
        iterElem n [] _ = -1
        iterElem n (x:xs) _ = iterElem (n-1) xs x

pullElem' :: Int -> [a] -> Maybe a
pullElem' n xs
  | n < 0 = Nothing
  | otherwise = iterElem' (n+1) xs Nothing
    where
        iterElem' 0 _ accum = accum
        iterElem' n [] _ = Nothing
        iterElem' n (x:xs) _ = iterElem' (n-1) xs (Just x)