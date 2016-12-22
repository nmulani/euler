-- Project Euler Problem Six
-- Find difference between sum of the squares of the first 100 natural #s
-- and the square of the sum of the first 100 natural #s

differenceSumSquare :: Integer
differenceSumSquare = abs((foldr (+) 0 (map (\n -> n^2) [1..100])) - ((foldr (+) 0 [1..100])^2))
