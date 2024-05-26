import Data.Maybe
import Data.List

-- LONGEST PATH
type Graf a b = [(a, [(a,b)])]

g :: Graf String Int
g = [
        ("a", [("b", 1), ("c", 1), ("f", 3)]),
        ("b", [("d",2)]),
        ("c", [("b",2),("e",1)]),
        ("d", [("c",2)]),
        ("e", [("d",3)]),
        ("f", [("c",4),("e",2)])
    ]

nc :: (Eq a, Ord b, Num b) => Graf a b-> a -> a -> Maybe ([a], b)
nc g from to
    | null paths = Nothing
    | otherwise = Just (from:longestPath, longestLen)
        where
            paths = allPaths g from to
            lengths = map (foldr (\(vertex,edgeLen) (list,total) -> (vertex:list, total+edgeLen)) ([],0)) paths
            (longestPath, longestLen) = maximumBy (\a b -> compare (snd a) (snd b)) lengths


allPaths :: Eq a => Graf a b -> a -> a -> [[(a, b)]]
allPaths g from to
    | from == to = [[]]
    | isNothing possibleEdges || null jPossibleEdges = []
    | otherwise = paths

        where
            possibleEdges = lookup from g
            jPossibleEdges = fromJust possibleEdges
            newG = graphWithoutVertex g from
            possiblePaths = map (\(v, len) -> map ((v,len):) (allPaths newG v to)) jPossibleEdges
            paths = concat possiblePaths


graphWithoutVertex :: Eq a => Graf a b -> a -> Graf a b
graphWithoutVertex g v = newG
    where
        noV = filter ((/= v) . fst) g
        newG = map (\(vertex, edges) -> (vertex, filter ((/= v) . fst) edges)) noV

-- ARRAY
class Foldable m => Pole m where
    make :: [t] -> m t
    get :: m t -> Int -> t
    update :: m t -> Int -> t -> m t

data FPole t = Nil | Item (FPole t) (Int, t) (FPole t)
    deriving Show

instance Pole FPole where
    make :: [t] -> FPole t
    make [] = Nil
    make l = helper l 0
        where
            helper [] _ = Nil
            helper list from = Item (helper left from) (from+half, middle) (helper right (from+half+1))
                where 
                    len = length list
                    half = len `div` 2
                    left = take half list
                    (middle:right) = drop half list


    get :: FPole t -> Int -> t
    get Nil _ = error "out of bounds."
    get (Item l (i, item) r) index 
        | i == index = item
        | index < i = get l index
        | index > i = get r index

    update :: FPole t -> Int -> t -> FPole t
    update Nil _ _ = error "out of bounds."
    update (Item l (i, item) r) index newItem
        | index == i = Item l (i, newItem) r
        | index < i = Item (update l index newItem) (i, item) r
        | index > i = Item l (i, item) (update r index newItem)

instance Foldable FPole where
    foldr :: (a -> b -> b) -> b -> FPole a -> b
    foldr _ b Nil = b
    foldr f b (Item l (index, item) r) = foldr f newB' l
        where
            newB = foldr f b r 
            newB' = f item newB

instance Functor FPole where
    fmap :: (a -> b) -> FPole a -> FPole b
    fmap f Nil = Nil
    fmap f (Item l (index, item) r) = Item (fmap f l) (index, f item) (fmap f r) 