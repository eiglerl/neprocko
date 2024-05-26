{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
import Data.List
import Data.Maybe

-- LONGEST PATH IN ORIENTED GRAPH
type Graph a b = [(a,[(a, b)])]
nc :: (Eq a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
nc [] _ _ = Nothing
nc g start end =
        -- no solution found
        if null lens then
            Nothing
        else
            -- add `start` vertex and remove "utility" item at the end of path
            Just (start:init maxPath, maxLen)
    where
        -- find all possible paths from `start` to `end`
        paths = allPaths g start end
        -- calculate lengths
        lens = map (\p -> (map fst p, foldr (\i s -> snd i + s) 0 p)) paths
        -- find the longest path
        (maxPath, maxLen) = maximumBy (\a b -> compare (snd a) (snd b)) lens


allPaths :: (Eq a, Ord b, Num b) => Graph a b -> a -> a -> [[(a,b)]]
allPaths g start end
    -- End of path (so that recursion can utilize `:`)
    | start == end = [[(end,0)]]
    -- Bad path
    | isNothing edges || null (fromJust edges) = []
    -- Recursion
    | otherwise = updatedSolutions
        where
            -- Find edges from `start`
            edges = lookup start g
            jEdges = fromJust edges
            -- prepare graph without `start`
            newG = graphWithoutVertex g start
            -- find all possible solutions starting from all vertices connected to `start`
            solutions = map (\e -> allPaths newG (fst e) end) jEdges
            -- add `(start,weight)` before every solution and concat (so that the solutions are not partitioned by vertices)
            updatedSolutions = concatMap (\(s, e) -> map (e:) s) (zip solutions jEdges)

graphWithoutVertex :: Eq a => Graph a b -> a -> Graph a b
graphWithoutVertex g v = noVEdges
    where
        -- Remove vertex v from fst
        noV = filter (\x -> fst x /= v) g
        -- Remove any edges leading to v
        noVEdges = map (\(a, edges) -> (a, filter (\x -> fst x /= v) edges)) noV

g :: Graph String Int
g = [
        ("a", [("b", 1), ("c", 1), ("f", 3)]),
        ("b", [("d", 2)]),
        ("c", [("b", 2), ("e", 1)]),
        ("d", [("c", 2)]),
        ("e", [("d", 3)]),
        ("f", [("c", 4), ("e", 2)])
    ]

-- ARRAY
class Foldable m => Pole m where
    make :: [t] -> m t
    get :: m t -> Int -> t
    update :: m t -> Int -> t -> m t


data FPole t = ArrayNil | FItem (FPole t) (Int, t) (FPole t) deriving Show


instance Pole FPole where
    make :: [t] -> FPole t
    make [] = ArrayNil
    make l = helper l 0
        where
            helper [] _ = ArrayNil
            helper [a] start = FItem ArrayNil (start, a) ArrayNil
            helper list start = FItem (helper left start) (half, middle) (helper right (half+1))
                where
                    len = length list
                    half = len `div` 2
                    left = take half list
                    (middle:right) = drop half list

    get :: FPole t -> Int -> t
    get ArrayNil i = error "index out of bounds"
    get (FItem l (n, item) r) i
        | n == i = item
        | n > i = get l i
        | otherwise = get r i

    update :: FPole t -> Int -> t -> FPole t
    update (FItem l (n, item) r) index newItem
        | n == index = FItem l (n, newItem) r
        | n > index = FItem l (n, item) (update r index newItem)
        | otherwise = FItem (update l index newItem) (n, item) r


instance Foldable FPole where
    foldMap f ArrayNil = mempty
    foldMap f (FItem l (n, item) r) = foldMap f l <> f item <> foldMap f r
    -- foldr f b ArrayNil = b
    -- foldr f b (FItem l (n, item) r) =   


instance Functor FPole where
    fmap f ArrayNil = ArrayNil
    fmap f (FItem l (n, item) r) = FItem (fmap f l) (n, f item) (fmap f r)