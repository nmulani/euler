-- Solving Euler problem #1
-- Updated to use fold and filter
-- To solve, load in Prelude and type solver [1,2..999]

module EulerOneV2 where

solver :: [Integer] -> Integer
solver xs = foldl (+) 0 (filter (\n -> (mod n 3 == 0) || (mod n 5 == 0))  xs)
