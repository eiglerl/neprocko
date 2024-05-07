{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.List
{-# HLINT ignore "Use camelCase" #-}
subsets :: [a] -> [[a]]
subsets list = [[]] ++ subsets_helper' list 1 [[]]

subsets_helper :: [a]  -> [[a]]
subsets_helper [] = [[]]
subsets_helper list = sets ++ map (++ [x]) sets
    where
        i = length list
        x = head (drop (i-1) list)
        sets = subsets_helper (take (i-1) list)

subsets_helper' :: [a] -> Int -> [[a]] -> [[a]]
subsets_helper' [] _ _ = []
subsets_helper' _ 0 _ = [[]]
subsets_helper' list i acc =
    let
        shortenedList = take i list
    in
        if length shortenedList < i
            then []
        else
            let
                x = head (drop (i-1) list)
                newAcc = map (++ [x]) (subsets_helper (take (i-1) shortenedList))
            in
                newAcc ++ subsets_helper' list (i+1) newAcc


combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs

-- combinations' :: Int -> [a] -> [[a]]
-- combinations' i list = inits list

pairs :: [a] -> [b] -> [(a, b)]
pairs [] _ = []
pairs _ [] = []
-- pairs (x:xs) (y:ys) = [(x, y)] ++ [(x, y') | y' <- ys] ++ [(x', y) | x' <- xs] ++ pairs xs ys
-- pairs (x:xs) (y:ys) = [(x,y') | y' <- (y:ys)] ++ pairs xs (y:ys)
pairs (x:xs) ys = map (x,) ys ++ pairs xs ys


-- allPairs :: [a] -> [b] -> [(a, b)]
-- allPairs [] _ = []
-- allPairs _ [] = []
-- allPairs (x:xs) (y:ys) = (x, y) : (zipWith (\a b -> (x, b)) xs ys ++ zipWith (\a b -> (a, y)) xs ys ++ allPairs xs ys)

allPairs :: [a] -> [b] -> [(a, b)]
allPairs [] _ = []
allPairs _ [] = []
allPairs (x:xs) (y:ys) = (x, y) : interleave (map (\b -> (x, b)) ys) (map (\a -> (a, y)) xs) ++ allPairs xs ys
    where
        interleave [] ys = ys
        interleave (x:xs) ys = x : interleave ys xs

zig :: [x] -> [y] -> [(x, y)]
zig xs ys = go [] xs where
    go rev [] =  concat [ zip rev ys' | ys' <- tails ys ]
    go rev (x : xs) = zip rev ys ++ go (x : rev) xs

pairs' xs ys = [(x, y) | (x,y) <- zig xs ys]


-- pairs :: [a] -> [b] -> [(a, b)]
-- pairs xs ys = pairs' xs ys
--     where
--         pairs' [] _ = []
--         pairs' _ [] = []
--         pairs' (x:xs) (y:ys) = (x, y) : rest
--             where
--                 rest = [(x', y') | x' <- xs, y' <- y:ys] ++ pairs' xs ys
-- subsets' list i = sets ++ map (++ [x]) sets
--     where 
--         i = length list
--         x = head (drop (i-1) list)
--         sets = subsets' (take (i-1) list)

-- subsets'' :: [a] -> Int -> [[a]]
-- subsets'' list i = map (++ [x]) sets
--     where 
--         x = head (drop (i-1) (take i list))
--         sets = subsets' (take (i-1) list) 
-- subsets'' list i = 
--     let shortenedList = take i list
--     in
--         if length shortenedList < i
--             then
--                 [[]]
--         else
--             map (++ [x]) sets
--                 where
--                     x = head (drop (i-1) shortenedList)
--                     sets = subsets' (take (i-1) shortenedList)


-- subsets'' list 0 sets = subsets'' list 1 [[]]
-- -- subsets'' list i sets = map (++ [x]) sets
-- --     where x = head (drop (i-1) (take i list))
-- subsets'' list 5 sets = sets
-- subsets'' list i sets = 
--     subsets'' list (i+1) newSets
--     where 
--         x = head (drop (i-1) (take i list))
--         newSets = map (++ [x]) sets




-- subsets' list i = 
--     if i <= length list 
--         then
--             -- all_subsets (take i list) ++ subsets' list (i+1)
--             map (head (drop (i-1) (take i list)) :) (all_subsets (take (i-1) list)) ++ subsets' list (i+1) 
--     else
--         [[]]

-- subsets' _ 0 = [[]]
-- subsets' list i = subsets' list (i-1) ++ sub
--     where sub = subsets_of_size i list




-- all_subsets list = foldl (++) [[]] [combinations i list | i <- [0..length list]]







