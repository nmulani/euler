-- is it prime = is it divisible by any prime numbers smaller than it?
-- Euler Problem 3
-- the trick here was figuring out that I only need to test divisibility
-- between x and sqrt(x). saves a ton of computing time!

lPF:: Integer -> Integer
-- find all divisors of x which are also prime, and return the greatest one
lPF x = last (filter (\n -> (mod x n == 0) && (isPrime n)) [2..(floor (sqrt (fromIntegral x :: Double)) )])


isPrime :: Integer -> Bool
isPrime 2 = True
isPrime 3 = True
-- let's try dividing x by all numbers between its square root and two (map statement)
-- if we don't receive a remainder for any of these numbers left in the mapped list
-- that means that x was not prime. (a.k.a, if there are any zeroes in the array once
-- the mod function has been mapped to it)
isPrime x = ((length (filter (\n -> n == 0) (map (\y -> mod x y) [2..(floor (sqrt (fromIntegral x :: Double)) )]))) == 0)
