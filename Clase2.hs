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
    (+) :: Num a => (b -> a) -> (b -> a) -> b -> a
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

-- rewriting of composition. Using a different symbol.
myCompose :: (b -> c) -> (a -> b) -> a -> c
myCompose = \ f -> \ g -> \ x -> f (g x)

-- Good Fibonacci
fib :: Integer -> Integer
fib n
  | n < 0 = -1
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fibIter n 1 1
      where
        fibIter 1 small big = big
        fibIter n small big = fibIter (n-1) big (small + big)

-- Bad Fibonacci, textually recursive.
badFib :: Integer -> Integer
badFib n
  | n < 0 = -1
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = badFib (n-1) + badFib (n-2)


null' :: [a] -> Bool
null' [] = True
null' _ = False

-- Pattern matchiing on lists
lsExample :: Num a => a
lsExample = let (x : (y : _)) = [3,5,7] ++ [11,13,17] in y

fiveIn3dPos :: (Num a, Eq a) => [a] -> Bool
fiveIn3dPos (_:(_:(5:_))) = True
fiveIn3dPos _ = False

boomOrBang x = if x < 10 then "BOOM!" else "BANG!"

boomBangs xs = [boomOrBang x | x <- xs, odd x]

removeNonUpperCase cs = [c | c <- cs , c `elem` ['A'..'Z']]


-- Our implementation of take
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n _
  | n <= 0 = []
take' n (x:xs) = x : take' (n-1) xs

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
  putStrLn (show x)
  printList xs

-- A COMPLICATED WAY TO BIND FIBS
--main = do
--  let nrs = [5 * n | n <- [1..5]]
--  fibs <- return $ map fib nrs
--  printList fibs

main = do
  let nrs = [5 * n | n <- [1..6]]
  putStrLn "This is good Fibonacci:"
  printList $ map fib nrs
  putStrLn "This is naive Fibonacci:"
  printList $ map badFib nrs