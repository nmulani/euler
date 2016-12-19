-- Project Euler Problem Four
-- Find the largest palindromic number that is a product of two three-digit numbers

-- How to search for palindrome without multiplying three-digit combinations
-- we've already tried previously? Create a list of unique pairs of three-digit
-- numbers:
-- i.e. [ (x,y) | x<-[1..10], y<-[1..10] ]


findPalindrome :: Integer
findPalindrome = maximum (filter (\n ->((show n)==(reverse (show n)))) (map (\x -> (fst x) * (snd x)) combos))
  where combos = [ (x,y) | x<-[100..999], y<-[100..999] ]
