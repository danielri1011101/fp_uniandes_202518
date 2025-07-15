sumNrs :: Integer -> Integer -> Integer
sumNrs a b
  | a > b = 0
  | otherwise = a + sumNrs (a+1) b

 

sumCubes a b
  | a > b = 0
  | otherwise = a^3 + sumCubes (a+1) b

sumPi :: Double -> Double -> Double
sumPi a b
  | a > b = 0
  | otherwise = expr a + sumPi (a+1) b
      where
        expr a = 1 / (4 * a + 1) / (4 * a + 3)


sumPi' :: Integer -> Integer -> Double
sumPi' a b
  | a > b = 0.0
  | otherwise = expr a + sumPi' (a+1) b
      where
        expr a = 1 / (4 * fromInteger a + 1) / (4 * fromInteger a + 3)

isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True

firstPrimes :: Integer -> [Integer] -> [Integer]
firstPrimes n []
  | n < 1 = []
  | n == 1 = [2]
firstPrimes n (x:xs)
  | n < 1 = []
  | otherwise = fndv (x:xs) : firstPrimes (n-1) xs
      where
        fndv = dropWhile (\ (==0) (`mod` k) )