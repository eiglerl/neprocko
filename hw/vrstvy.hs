import Data.List
import Data.Function
-- sampleMap :: Map.Map String Int
-- sampleMap = Map.fromList [("apple", 5), ("banana", 3), ("orange", 7)]
-- vrstvy :: Ord a => [(a,a)] -> [[a]]
-- vrstvy [] = []
-- -- vrstvy edges = sort leaves : (if null leaves then [map fst counts] else vrstvy newEdges)
-- -- vrstvy edges = sort leaves : sort [others] ++ vrstvy newEdges
-- vrstvy edges = sort leaves : if null others then sort $ vrstvy newEdges else sort [others] ++ vrstvy newEdges

--     where
--         -- helper [] layers = layers
--         -- helper edges layers = leaves : (helper newEdges)

--         counts = countsOfEdges edges
--         (leaves, others) = getLeaves counts
--         newEdges =  removeLeaves leaves edges

-- countsOfEdges :: Ord a => [(a,a)] -> [(a,Int)]
-- countsOfEdges edges =
--     helper edges [] []
--     where
--         add item map elements =
--             if item `elem` elements then
--                 (update item map [],elements)
--             else
--                 ((item,1):map, item:elements)

--         update item [] acc = acc
--         update item ((i,c):map) acc = update item map ((i,updatedC):acc)
--             where
--                 updatedC = if item == i then c+1 else c


--         helper [] map _ = map
--         helper ((a,b):edges) map elements = helper edges newMap' newElements'
--             where
--                 (newMap, newElements) = add a map elements
--                 (newMap', newElements') = add b newMap newElements

-- getLeaves :: Ord a => [(a,Int)] -> ([a],[a])
-- getLeaves items = (leaves, if not (null leaves) then [] else filter (not . (`elem` leaves)) (map fst items))
--     where
--         leaves = map fst (filter (\(_,c) -> c==1) items)

-- removeLeaves :: Ord a => [a] -> [(a,a)] -> [(a,a)]
-- -- removeLeaves leaves = filter (\x -> fst x `elem` leaves)
-- removeLeaves leaves items = filter (not . (`elem` leaves) . snd) (filter (not . (`elem` leaves) . fst) items)



betterRepresentation :: Ord a => [(a,a)] -> [(a, [a])]
betterRepresentation = map (\grp -> (fst (head grp), map snd grp)) . groupBy ((==) `on` fst) . sort . concatMap (\(a, b) -> [(a, b), (b, a)])


-- betterRepresentation :: Ord a => [(a,a)] -> [(a, [a])]
-- betterRepresentation edges = helper edges []
--     where
--         helper :: Ord a => [(a,a)] -> [(a, [a])] -> [(a, [a])]
--         helper [] list = list
--         helper (edge:edges) list = helper edges updatedList
--             where
--                 updatedList = update list (fst edge) (snd edge)

--         update :: Ord a => [(a,[a])] -> a -> a -> [(a,[a])]
--         update list item1 item2 =
--             [
--                 if a == item1 then (a, item2:neighbors)
--                 else if a == item2 then (a, item1:neighbors)
--                 else (a, neighbors)
--                     | (a, neighbors) <- listWithItems]
--             where
--                 listWithItem = if item1 `elem` map fst list then list else (item1,[]):list
--                 listWithItems = if item2 `elem` map fst listWithItem then listWithItem else (item2,[]):listWithItem

getLeaves :: [(a, [a])] -> [a]
getLeaves graph = map fst (filter (\(i, counts) -> null counts || null (tail counts)) graph)

removeLeaves :: Ord a => [(a, [a])] -> [a] -> [(a, [a])]
-- removeLeaves graph leaves = 
--     [(i, filter (`notElem` leaves) neighbors) | (i,neighbors) <- graph, i `notElem` leaves]
removeLeaves graph leaves = graphWithoutLeaves
    where
        noLeavesAsFst = filter (\(i, counts) -> i `notElem` leaves) graph
        graphWithoutLeaves = map (\(i, neighbours) -> (i, filter (`notElem` leaves) neighbours)) noLeavesAsFst

vrstvy :: Ord a => [(a,a)] -> [[a]]
vrstvy edges = helper better
    where
    better = betterRepresentation edges
    helper [] = []
    helper graph = sort leaves : helper withoutLeaves
        where
            leaves = getLeaves graph
            withoutLeaves = removeLeaves graph leaves

