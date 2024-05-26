import Data.Maybe
import Data.List
import Distribution.Compat.CharParsing (Parsing(unexpected))

type Graf = [(Int,[Int])]

g :: Graf
g = [(1,[3,6]), (2,[6]), (3,[2,5]),(4,[1,3]),(5,[2,4]),(6,[3,4,5])]

g2 :: Graf
g2 = [(1,[2,3]),(2,[1]),(3,[2])] 

-- allHc :: Graf -> [Int] -> Int -> [[Int]]
-- allHc g ends from 
--     | isNothing edges = []
--     | null g = [[]]
--     | null newG && from `elem` ends = [[from]]
--     | otherwise = concatSolutions
--     where
--         edges = lookup from g
--         jEdges = fromJust edges
--         newG = graphWithoutVertex g from
--         possibleSolutions = map (allHc newG ends) jEdges
--         concatSolutions = concatMap (map (from:)) possibleSolutions

-- hc :: Graf -> Maybe [Int]
-- hc g 
--     | null hcs = Nothing
--     | otherwise = Just $ head hcs
--     where
--         (start,ends) = head g
--         newG = graphWithoutVertex g start
--         hcs = allHc g ends start






graphWithoutVertex :: Graf -> Int -> Graf
graphWithoutVertex g v = noVEdges
    where
        noV = filter ((/= v) . fst) g
        noVEdges = map (\(vertex, edges) -> (vertex, filter (/= v) edges)) noV
    

hc :: Graf -> Maybe [Int]
hc g 
    | null hcs = Nothing
    | otherwise = Just (solution ++ [start])
    where
        (start, _) = head g
        ends = map fst (filter (\(v, edges) -> start `elem` edges) g)
        hcs = allHc g ends start
        solution = head hcs

allHc :: Graf -> [Int] -> Int -> [[Int]]
allHc g ends from 
    | null newG && from `elem` ends = [[from]]
    | isNothing edges || null jEdges = []
    | otherwise = updatedSolutions
    where
        edges = lookup from g
        jEdges = fromJust edges
        newG = graphWithoutVertex g from
        solutions = map (allHc newG ends) jEdges
        updatedSolutions = concatMap (map (from:)) solutions

-- QUEUE
class (Functor q) => PQueue q where
    empty :: q a
    insert' :: (a, Int) -> q a -> q a
    remove :: q a -> ((a, Int), q a)
    merge :: q a -> q a -> q a

data FQueue a = Queue [(Int, [a])]
    deriving Show

instance PQueue FQueue where
    empty :: FQueue a
    empty = Queue []
    
    insert' :: (a, Int) -> FQueue a -> FQueue a
    insert' (item, prio) (Queue []) = Queue [(prio, [item])]
    insert' (item, prio) (Queue ((p, list):rest))
        | prio == p = Queue ((p, list ++ [item]):rest)
        | prio < p  = Queue ((prio, [item]):((p, list):rest))
        | prio > p  = Queue ((p, list):inserted)
            where
                (Queue inserted) = insert' (item, prio) (Queue rest)

    remove :: FQueue a -> ((a, Int), FQueue a)
    remove (Queue []) = error "Empty queue."
    remove (Queue ((p,i:items):rest)) 
        | null items = ((i, p), Queue rest)
        | otherwise  = ((i, p), Queue ((p, items):rest))


    merge :: FQueue a -> FQueue a -> FQueue a
    merge (Queue []) (Queue p) = Queue p
    merge (Queue q) (Queue []) = Queue q 
    merge (Queue ((p1,list1):rest1)) (Queue ((p2,list2):rest2))
        | p1 == p2 = Queue ((p1, list1 ++ list2) : mergedSame)
        | p1 < p2  = Queue ((p1, list1) : mergedP1Smaller)
        | p1 > p2  = Queue ((p2, list2) : mergedP2Smaller)
            where
                (Queue mergedP1Smaller) = merge (Queue rest1) (Queue ((p2,list2):rest2))
                (Queue mergedP2Smaller) = merge (Queue ((p1,list1):rest1)) (Queue rest2)
                (Queue mergedSame) = merge (Queue rest1) (Queue rest2)

instance Functor FQueue where
    fmap :: (a -> b) -> FQueue a -> FQueue b
    fmap f (Queue []) = Queue []
    fmap f (Queue ((p, list):rest)) = Queue ((p, map f list) : mapped)
        where
            (Queue mapped) = fmap f (Queue rest) 

instance Foldable FQueue where
    foldr :: (a -> b -> b) -> b -> FQueue a -> b
    foldr _ b (Queue []) = b
    foldr f b (Queue ((p, list):rest)) = newB'
        where
            newB = foldr f b (Queue rest)
            newB' = foldr f newB list