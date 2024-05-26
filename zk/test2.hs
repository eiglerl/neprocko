import Data.List


data Tree a = Nil | Vertex a [Tree a]
    deriving Show

fold :: (a -> [b] -> b) -> b -> Tree a -> b
fold _ b Nil = b
-- fold f b (Vertex a (c:children)) = f a (fold f b c)
fold f b (Vertex a children) = f a (map (fold f b) children)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Nil = Nil
mapTree f (Vertex val children) = Vertex (f val) (map (mapTree f) children)

arita :: Tree a -> Int
arita = fold (\_ childrenArity -> max (length childrenArity) (maximum childrenArity)) 0

takeTree :: Tree a -> Int -> Tree a
takeTree Nil _ = Nil
takeTree (Vertex a children) h
    | h == 0 = Vertex a [Nil]
    | h > 0 = Vertex a (map (\c -> takeTree c (h-1)) children)
    | otherwise = Nil

infTree :: Tree Int
infTree =
    Vertex 1 [infTree]


t :: Tree Int
t =
    Vertex 5 [
        Vertex 1 [
            Vertex 2 [Nil],
            Vertex 3 [
                Vertex 4 [Nil]
            ]
        ],
        Vertex 6 [Nil],
        Vertex 7 [Nil],
        Vertex 8 [Nil],
        Vertex 9 [Nil],
        Vertex 10 [
            Vertex 11 [
                Vertex 12 [Nil],
                Vertex 13 [Nil]
            ],
            Vertex 14 [Nil]
        ]
    ]





-- type Mapa = [((Int, Int), [(Int, Int)])]
-- B)
type Mapa a = [((a, a), [(a, a)])]
-- C) a needs to be Ord bcs of comparison
zdroje :: Ord a => Mapa a -> [((a, a), (a, a))]
zdroje mapa = sorted
    where
        filtered = map (\((locX, locY), connected) -> ((locX, locY), filter (\(conX, conY) -> conX <= locX && conY <= locY)  connected)) mapa
        sorted = map (\(loc, connected) -> (loc, minimumBy (\a b -> compare b a) connected)) filtered

-- D) added function for X comparison
zdroje' :: Ord a => Mapa a -> (a -> a -> Bool) -> [((a, a), (a, a))]
zdroje' mapa f = sorted
    where
        filtered = map (\((locX, locY), connected) -> ((locX, locY), filter (\(conX, conY) -> f locX conX && conY <= locY)  connected)) mapa
        sorted = map (\(loc, connected) -> (loc, minimumBy (\a b -> compare b a) connected)) filtered


map1 = [
    ((0, 0), [(0,0), (2, 1), (3, 3)]),
    ((1, 3), [(2, 1), (0, 0)]),
    ((2, 1), [(3, 2), (0, 0)]),
    ((3, 2), [(3, 3), (2, 1), (1, 3)]),
    ((3, 3), [(3, 2), (1, 3)])
    ]

map2 = [
    ((0, 0), [(0,0),(2, 1), (3, 3)]),
    ((1, 3), [(2, 1), (0, 0)]),     -- tady je rozdil, pujde videt v posledni polozce vysledku
    ((2, 1), [(3, 2), (0, 0)]),
    ((2, 0), [(0, 0)]),
    ((3, 2), [(3, 3), (2, 1), (1, 3)]),
    ((3, 3), [(3, 2), (1, 3)])
    ]