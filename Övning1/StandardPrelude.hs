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

smallestFactor:: Int -> Int
smallestFactor n 
	| n == 0 = 0
	| n == 1 = 1
	| n == 2 = 2
	| otherwise = nextFactor 1 n

nextFactor:: Int -> Int -> Int 
nextFactor k n 
	| mod n (k+1) == 0 = k+1
	| otherwise = nextFactor (k+1) n