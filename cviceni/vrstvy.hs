import Data.List

type Graph a = [(a, [a])]

neighbors :: (Ord a) => [(a,a)] -> [(a,[a])]
neighbors edges =
    map (\a -> (fst $ head a, map snd a)) $ groupBy (\a b -> fst a == fst b) $ sort $ concatMap (\(a,b) -> [(a,b),(b,a)]) edges

layer :: Ord a => Graph a -> ([a], Graph a)
layer graph = (leaves, map (\(v, e) -> (v, leaves \\ e)) newGraph)
    where 
    (almostLeaves, newGraph) = partition (\(v, e) -> length e <= 1) graph
    leaves = map fst almostLeaves

vrstvy :: Ord a => [(a,a)] -> [[a]]
vrstvy graph = vrstvy' newGraph
    where 
        newGraph = neighbors graph

vrstvy' :: Ord a => Graph a -> [[a]]
vrstvy' [] = []
vrstvy' g = leaves : vrstvy' newGraph
    where
        (leaves, newGraph) = layer g


