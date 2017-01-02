-- Euler Problem 7
-- We're going to resuse our isPrime method from Euler Three and create a recursive solver from it
-- to solve, compile and run findPrime (1,2) -- 1 is the number of prime, 2 is the first prime number
findPrime :: (Integer, Integer) -> Integer
findPrime (x,y)
  | (x==10001 && isPrime y) = y
  | (isPrime y) = findPrime (x+1, y+1)
  | otherwise = findPrime (x, y+1)

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime 3 = True
-- let's try dividing x by all numbers between its square root and two (map statement)
-- if we don't receive a remainder for any of these numbers left in the mapped list
-- that means that x was not prime. (a.k.a, if there are any zeroes in the array once
-- the mod function has been mapped to it)
isPrime x = ((length (filter (\n -> n == 0) (map (\y -> mod x y) [2..(floor (sqrt (fromIntegral x :: Double)) )]))) == 0)
