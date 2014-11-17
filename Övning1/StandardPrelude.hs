maxi :: Ord a => a -> a -> a
maxi x y
	| x <= y = y
	| otherwise = x

sumsq :: Int -> Int
sumsq n
	| n <= 0 = 0
	| otherwise = n * n + (sumsq (n - 1))

sumsq' :: Int -> Int
sumsq' n
	| n <= 0 = 0
	| otherwise = foldl (+) 0 (map (^2) [1..n])

hanoi :: Float -> Float
hanoi n
	| n <= 0 = 0
	| otherwise = 1 + 2 * hanoi (n-1)