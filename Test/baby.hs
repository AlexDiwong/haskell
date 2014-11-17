doubleMe x = x + x

doubleUS x y = doubleMe x  + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

triples = [ (a,b,c) | c <- [1..10],  a <- [1..c], b <- [1..a], a ^ 2 + b ^ 2 == c ^ 2, a + b + c == 24]

circumFerence :: Float -> Float
circumFerence n = 2 * pi * n

circumFerence' :: Double -> Double
circumFerence' n = 2 * pi * n

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' ( x : _ ) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstletter [] = "Empty String"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height
	| bmi <= 18.5 = "You're underweight, you emo, you!"
	| bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
	| bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
	| otherwise   = "You're a whale, congratulations!"
	where bmi = weight / height ^ 2 