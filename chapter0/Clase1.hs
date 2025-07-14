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

myReverse :: [a] -> [a]
myReverse xs = iterReverse xs []
  where
    iterReverse [] accum = accum
    iterReverse (x:xs) accum = iterReverse xs (x:accum)

myConcat :: [a] -> [a] -> [a]
myConcat [] ys = ys
myConcat (x:xs) ys = x : (myConcat xs ys)

-- With pattern matching
describeList :: [a] -> String
describeList [] = "Empty list."
describeList [x] = "Singleton list."
describeList _ = "Longer list."

-- With case expressions.
describeList' :: [a] -> String
describeList' xs =
    case xs of [] -> "Empty list."
               [x] -> "Singleton list."
               _ -> "Longer list."

compareLists :: [a] -> [a] -> String
compareLists xs ys =
    let (a,b) = (length xs, length ys)
    in talk (a-b)
      where
        talk n
          | n > 0 = "Left list is bigger!"
          | n == 0 = "Same length!"
          | n < 0 = "Right list is bigger!"