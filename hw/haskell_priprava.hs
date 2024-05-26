{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
import Data.List
import Data.Char
import Data.Maybe

-- PRIMES
factors :: Int -> [Int]
factors n = factors' n n 2
    where
        factors' realN n x
            | x * x <= realN =
                if n `mod` x == 0 then
                    x : factors' realN (n `div` x) x
                else
                    factors' realN n (x+1)
            | n > 1 = [n]
            | otherwise = []

gap :: Int -> (Int, Int)
gap = gap' 2
    where
        gap' :: Int -> Int -> (Int, Int)
        gap' min max = foldr (\(a1,a2) (b1,b2) -> if b2 - b1 > a2 - a1 then (b1,b2) else (a1,a2)) (0,0) (zip allPrimes (tail allPrimes))
            where
                allPrimes = [i | i <- [min..max], length (factors i) == 1]

--  RELATIONS
isReflexive :: (a -> a -> Bool) -> [a] -> Bool
isReflexive f = all (\x -> f x x)

isSymmetric :: (a -> a -> Bool) -> [a] -> Bool
isSymmetric f list = and [not (f x y) || f y x  | x <- list, y <- list]

isTransitive :: (a -> a -> Bool) -> [a] -> Bool
isTransitive f list = and [not (f x y) || not (f y z) || f x z | x <- list, y <- list, z <- list]

isEquiv :: (a -> a -> Bool) -> [a] -> Bool
isEquiv f list = isReflexive f list && isSymmetric f list && isTransitive f list

classes :: (a -> a -> Bool) -> [a] -> [[a]]
classes f [] = []
classes f (l:list) = fst both : classes f (snd both)
    where
        both = partition (f l) (l:list)

reflexiveClosure :: Eq a => (a -> a -> Bool) -> (a -> a -> Bool)
reflexiveClosure f = g
    where
        g a b = f a b || a == b

-- WORD SEARCH
search :: [String] -> [String] -> [Int]
search table words = sumLists (map (countsInTable loweredTable) loweredWords) (map (countsInTable (transpose loweredTable)) loweredWords)
    where
        loweredTable = map (map toLower) table
        loweredWords = map (map toLower) words
countsInTable :: [String] -> String -> Int
countsInTable table word = foldr (\r l -> sum [1 | tR <- tails r, word `isPrefixOf` tR || reverse word `isPrefixOf` tR] + l) 0 table

sumLists :: Num a => [a] -> [a] -> [a]
sumLists [] [] = []
sumLists (l1:rest1) (l2:rest2) = l1+l2 : sumLists rest1 rest2

test3 = let line = cycle "dandelion"
            grid = take 400 [take 400 s | s <- tails line]
        in search grid ["Dand", "LION", "iled", "noile", "dandy"]

-- ALL BALANCED BINARY TREES
data Tree = Nil | Node Tree Int Tree
    deriving (Eq, Ord, Show)

allBalanced :: Int -> [Tree]
allBalanced = helper 1
    where
        helper :: Int -> Int -> [Tree]
        helper min max
            | min == max = [Node Nil min Nil]
            | min < max =
                    [Node left (min + split + 1) right | split <- splits, left <- helper min (min + split), right <- helper (min + split + 2) max]
            | otherwise = [Nil]
                where
                    splits = possibleSplits (max - min - 1)


possibleSplits :: Integral a => a -> [a]
possibleSplits n
    | odd n = [middle]
    | otherwise = [middle-1, middle]

    where
        middle = n `div` 2

-- FUNCTIONAL QUEUE
class Queue q where
    emptyQueue :: q a
    isEmpty :: q a -> Bool
    enqueue :: a -> q a -> q a
    dequeue :: q a -> (a, q a)

newtype MyQueue a = MyQueue [a]

instance Queue MyQueue where
    emptyQueue :: MyQueue a
    emptyQueue = MyQueue []
    isEmpty :: MyQueue a -> Bool
    isEmpty (MyQueue q) = null q
    enqueue :: a -> MyQueue a -> MyQueue a
    enqueue item (MyQueue list) = MyQueue (list ++ [item])
    dequeue :: MyQueue a -> (a, MyQueue a)
    dequeue (MyQueue (l:list)) = (l, MyQueue list)
    dequeue _ = error "empty queue"



test =
    let q = emptyQueue :: MyQueue Int
        q1 = enqueue 5 q
        q2 = enqueue 10 q1
        e = isEmpty q2
        (x, q3) = dequeue q2
        (y, q4) = dequeue q3
        f = isEmpty q4
    in (e, x, y, f)


-- TAUTOLOGIES
data Prop =
    Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  deriving Show

type Model = [(Char, Bool)]

isTaut :: Prop -> Bool
isTaut p = all (eval p) models
    where
        vars = getVars p
        models = getModels vars


getVars :: Prop -> [Char]
getVars p = sort (getVars' p)
    where
        getVars' (Var a) = [a]
        getVars' (Not p) = getVars' p
        getVars' (And p q) = getVars' p ++ getVars' q
        getVars' (Or p q) = getVars' p ++ getVars' q

getModels :: [Char] -> [Model]
getModels [] = []
getModels [v] = [[(v,True)],[(v,False)]]
getModels (v:vars) = map ((v,True):) rest ++ map ((v,False):) rest
    where
        rest = getModels vars

eval :: Prop -> Model -> Bool
eval (Var a) model = (a,True) `elem` model
eval (Not p) model = not (eval p model)
eval (And p q) model = eval p model && eval q model
eval (Or p q) model = eval p model || eval q model

