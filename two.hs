-- Solving Euler problem #2
-- To solve, compile and enter 'solver limitedFibs'

module EulerTwo where


-- [1,2,3,5,8,13,21,34...]
-- Add the two prior numbers in the sequence to achieve
-- the next number in the Fibbonaci sequence

-- with some help from https://wiki.haskell.org/The_Fibonacci_sequence
-- and http://stackoverflow.com/questions/10336392/finite-comprehension-of-an-infinite-list
-- and solver code adapted from one.hs!

fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t

limitedFibs = takeWhile (< 4000000) fibs

solver :: [Integer] -> Integer
solver [] = error "Empty list"
solver [x] = if (mod x 2 == 0)
  then
    x
  else
    0
solver (x:xs) = (+) (solver [x]) (solver xs)
