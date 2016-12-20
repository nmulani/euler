-- Project Euler Problem Five
-- What is the smallest positive number evenly divisible by all of [1..20]
-- We know 2520 is the smallest divided evenly by [1..10]
-- let's eliminate any numbers from [10..20] that overlap in factors
-- which means we only need to check prime factors from the new list

-- we can start with 2520, and increment by 2520, because this new number must
-- share all those same factors
-- type in findDivisibleNum 2520
findDivisibleNum :: Integer -> Integer
findDivisibleNum x
  | all (\y->(mod x y == 0)) [11, 13, 14, 16, 17, 18, 19, 20] = x
  | otherwise = findDivisibleNum $! (x+2520)

-- tricky part here was figuring out that i needed to increment
-- prior to tail recursion by using '$!'
-- otherwise, we accumulate a very large list of operations
-- to do afterwards, which causes stack overflow
