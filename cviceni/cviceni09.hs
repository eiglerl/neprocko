-- insert :: (Ord a) => a -> [a] -> [a]
-- insert x ys = foldr (\a (smaller,larger) -> if a < x then (a:smaller,larger) else (smaller,[x])) ([],[]) ys
insert x = foldr (\a new -> if a < x then a:new else x:a:new) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\a l -> if f a then a:l else l) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\a l -> if f a then a:l else []) []

test = foldr (\(a,b) c -> (a*b):c) [] (zip [1..5] [1..5])

