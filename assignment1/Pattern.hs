module Pattern where
import Utilities


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ _ [] = []
substitute _ [] _ = []
substitute w b c = foldl (\acc x -> if x == w then acc ++ c else acc++[x]) [] b 


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ _ [] = Nothing
match _ [] _ = Nothing
match w a b 
	| (head a) == (head b) = match w (tail a) (tail b)
	| (head a) == w = orElse (singleWildcardMatch a b) (longerWildcardMatch a b)
	| otherwise = Nothing



-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = if (ps == [] || xs == [] || head ps == head xs) then mmap (const [x]) (match wc ps xs) else Nothing
longerWildcardMatch (wc:ps) (x:xs) = if (ps == [] || xs == [] || ps /= xs) then mmap ([x] ++) (match wc (wc:ps) xs) else  Nothing



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
transformationApply w f patt ([],[]) = Nothing
transformationApply w f patt (first, second) = mmap (substitute w second) (match w first (f patt))


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply w f (p:ps) s = orElse (transformationApply w f s p) (transformationsApply w f ps s)

