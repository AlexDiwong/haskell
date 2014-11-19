module Pattern where
import Utilities


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute w b c 
	| b == [] = []
	| c == [] = b
	| otherwise = foldl (\acc x -> if x == w then acc ++ c else acc++[x]) [] b


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match w a b 
	| a == [] && b == [] = Just []
	| a == [] || b == [] = Nothing
	| (head a) == (head b) = match w (tail a) (tail b)
	| (head a) == w = orElse (singleWildcardMatch a b) (longerWildcardMatch a b)
	| otherwise = Nothing



-- Helper function to match
singleWildcardMatch (a:as) (b:bs) 
	| as == [] || bs == [] || head as == head bs = mmap (const [b]) (match a as bs)
	| otherwise = Nothing

longerWildcardMatch (a:as) (b:bs) 
	| as == [] || bs == [] || as /= bs = mmap ([b] ++) (match a (a:as) bs)
	| otherwise = Nothing



-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions


-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply w patt (first, second)
	| first == [] && second == [] = Nothing
	| otherwise = mmap (substitute w second . )(match w first patt)


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply w f p s
	| p == [] = Nothing
	| otherwise = orElse (transformationApply w f s (head p)) (transformationsApply w f (tail p) s)


