module Utilities where

map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)
-- maybe -> x (default value) id (a function) f(x) (Maybe value)
-- If the f(x) is Nothing, the function returns x
--Otherwise, it applies id to the value inside the Just f(x) and returns the result.

fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
