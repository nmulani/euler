-- Solving Euler problem #1
-- To solve, load in Prelude and type solver [1,2..999]

module EulerOne where

solver :: [Integer] -> Integer
solver [] = error "Empty list"
solver [x] = if ((mod x 3 == 0) || (mod x 5 == 0))
  then
    x
  else
    0
solver (x:xs) = (+) (solver [x]) (solver xs)
