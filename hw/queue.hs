{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.List

class Queue q where
    emptyQueue :: q a
    isEmpty :: q a  -> Bool
    enqueue :: a -> q a -> q a
    dequeue :: q a -> (a, q a)

newtype SimpleQueue a = SimpleQueue [a]

instance Queue SimpleQueue where
    emptyQueue = SimpleQueue []
    isEmpty (SimpleQueue []) = True
    isEmpty _ = False
    enqueue x (SimpleQueue xs) = SimpleQueue (xs ++ [x])
    dequeue (SimpleQueue []) = error "Empty queue"
    dequeue (SimpleQueue (x:xs)) = (x, SimpleQueue xs)


testA =
    let q = emptyQueue :: SimpleQueue Int
        q1 = enqueue 5 q
        q2 = enqueue 10 q1
        e = isEmpty q2
        (x, q3) = dequeue q2
        (y, q4) = dequeue q3
        f = isEmpty q4
    in (e, x, y, f)

data SQueue a = SQueue [a] [a]

instance Queue SQueue where
    emptyQueue = SQueue [] []
    isEmpty (SQueue [] []) = True
    isEmpty _ = False
    enqueue x (SQueue front back) = SQueue front (x:back)
    dequeue (SQueue [] []) = error "Empty queue"
    dequeue (SQueue [] back) = dequeue (SQueue (reverse back) [])
    dequeue (SQueue (f:front) back) = (f, SQueue front back)

instance Eq a => Eq (SQueue a) where
    (SQueue front1 back1) == (SQueue front2 back2) = list1 == list2
        where
            list1 = front1 ++ reverse back1
            list2 = front2 ++ reverse back2

instance Show a => Show (SQueue a) where
    show (SQueue front back) = "q" ++ show (front ++ reverse back)

instance Functor SQueue where
    fmap f (SQueue front back) = SQueue (map f front) (map f back)

queue_of_nums :: Queue q => Int -> Int -> q Int
queue_of_nums from to = queue_from_list (reverse [from..to])

queue_from_list :: Queue q => [a] -> q a
queue_from_list = foldr enqueue emptyQueue
