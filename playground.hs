-- 
-- Haskell Playground
-- 

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let 
		sideArea = 2 * pi * r * h
		topArea = pi * r ^ 2
	in
		sideArea + 2 * topArea


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


repeat' :: a -> [a]
repeat' a = [a] ++ repeat' a


quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let 
    smallerThanXSorted = quicksort' [a | a <- xs, a <= x]
    biggerThanXSorted = quicksort' [a | a <- xs, a > x]
  in 
    smallerThanXSorted ++ [x] ++ biggerThanXSorted


length' :: (Num b) => [a] -> b
length' [] = 0
length' (x:xs) = 1 + length' xs

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

