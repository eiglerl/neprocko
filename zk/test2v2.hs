import Data.List
import Language.Haskell.TH (conE)
import GHC.RTS.Flags (RTSFlags(concurrentFlags))

data Tree a = Nil | Node a [Tree a]
    deriving Show

fold :: (a -> [b] -> b) -> b -> Tree a -> b
fold _ b Nil = b
fold f b (Node a children) = f a newB
    where
        newB = map (fold f b) children

arita :: Tree a -> Int
arita Nil = 0
arita (Node a children) = fold (\_ ch -> max (length ch) (maximum ch)) 0 (Node a children)

t :: Tree Int
t =
    Node 5 [
        Node 1 [
            Node 2 [Nil],
            Node 3 [
                Node 4 [Nil]
            ]
        ],
        Node 6 [Nil],
        Node 7 [Nil],
        Node 8 [Nil],
        Node 9 [Nil],
        Node 10 [
            Node 11 [
                Node 12 [Nil],
                Node 13 [Nil]
            ],
            Node 14 [Nil]
        ]
    ]

t2 :: Tree Int
t2 =
    Node 1 [
        Node 2 [
            Node 3 [Nil],
            Node 4 [Nil],
            Node 5 [Nil],
            Node 6 [Nil]
        ]
    ]

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Nil = Nil
mapTree f (Node a children) = Node (f a ) (map (mapTree f) children)

takeTree :: Int -> Tree a -> Tree a
takeTree _ Nil = Nil
takeTree 0 (Node a children) = Node a [Nil]
takeTree h (Node a children) = Node a (map (takeTree (h-1)) children)

inf :: Tree Int
inf = inf' 1
    where
        inf' i = Node i [inf' (i+1)]



-- ZDROJE
type Mapa a = [((a,a), [(a,a)])]

zdroje :: (Ord a, Eq a, Num a) => Mapa a -> [((a, a), (a, a))]
zdroje m = connected
    where
        smaller = map (\((locX, locY), neighbors) -> ((locX,locY), filter (\(nX, nY) -> nX <= locX && nY <= locY) neighbors)) m
        (vodojem, others) = partition (\((locX, locY), _) -> locX == 0 && locY == 0) smaller
        connected = map (\((locX, locY), neighbors) -> ((locX, locY), maximum neighbors)) others

map1 :: Mapa Int
map1 = [
    ((0, 0), [(2, 1), (3, 3)]),
    ((1, 3), [(2, 1), (0, 0)]),
    ((2, 1), [(3, 2), (0, 0)]),
    ((3, 2), [(3, 3), (2, 1), (1, 3)]),
    ((3, 3), [(3, 2), (1, 3)])
    ]

test =
    [1..5] >>= enumFromTo 1