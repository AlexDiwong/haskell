module Utilities where

-- Applies a tuple of functions on a tuple of arguments --
-- Returns a tuple --  
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Applies a function to a Maybe --
-- Returns only a just with applied function --
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- Returns the found object in a list else specified argument --
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- Returns x if not nothing, otherwise x with applied function f --
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- Recursive function that applies a function on an argument --
-- Stops when the applied function results in the original argument  --
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- Converts a realfrac number to an integer index and returns the element in the given list --
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

