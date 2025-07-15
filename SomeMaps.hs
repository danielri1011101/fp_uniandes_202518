sumIntegers :: Integer -> Integer -> Integer
sumIntegers a b
  | a > b = 0
  | otherwise = a + sumIntegers (a+1) b


sumCubes :: Integer -> Integer -> Integer
sumCubes a b
  | a > b = 0
  | otherwise = a^3 + sumCubes (a+1) b

sumPi :: Integer -> Double
sumPi n
  | n < 0 = 0
  | n == 0 = 1 / 3
  | otherwise = (follow n) + sumPi (n-1)
      where
        follow :: Integer -> Double
        follow n = 1 / ((4 * nf + 1) * (4 * nf + 3))
        nf = fromInteger n

--sum' :: Integer -> Integer -> (Integer -> Integer) -> (Integer -> Integer) -> Integer
sum' a b term next
  | a > b = 0
  | otherwise = if next a <= b then term a + sum' (next a) b term next else term a
