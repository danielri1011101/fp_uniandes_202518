-- Wolf's code

res1 = liftA2 (+) [1,2,3] [4,5,6] 
res2 = liftA2 (+) (Just 10) (Just 22)
res3 = liftA2 (+) (Right 10) (Right 22)

applicativeCode a1 a2 = liftA2 (+) a1 a2

res1' = applicativeCode [1,2,3] [4,5,6]
res2' = applicativeCode (Just 10) (Just 22)
res3' = applicativeCode (Right 10) (Right 22)

data Logged a = L a String deriving (Show)

instance Functor Logged where
  fmap f (L x s) = L (f x) s
  
instance Applicative Logged where
  pure = flip L ""
  (L f s) <*> (L a s') = L (f a) (s++s')
  
logg x m = L x m
  
se = sequenceA [pure (fac 3), 
                logg 40 "fourty",
                applicativeCode (pure 45) 
                                (logg 10 "ten"),
                logg (fac 3) "three"]
                
ff x = if even x then pure x else logg x (show x)

sw = sequenceA [ff x | x <- [1..10]]

--
-- Example of monadic (IO) code that cleverly uses the Applicative style
-- to apply pure functions (read) to monadic values (IO):
--

fac n = product [1..n]

main = do putStr "Give a number: "
          l <- read <$> getLine
          putStr "The fac is "
          putStrLn $ show $ fac l
