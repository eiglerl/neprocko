import Data.Bits (And(getAnd))
data Extended = MinusInfinity | Finite Integer | PlusInfinity
    deriving (Eq, Ord)

instance Show Extended where
    show MinusInfinity = "-inf"
    show (Finite x) = show x
    show PlusInfinity = "+inf"

leq :: Extended -> Extended -> Bool
leq MinusInfinity _ = True
leq _ PlusInfinity = True
leq (Finite x) (Finite y) = x <= y
leq _ _ = False

data Point = Point { getX :: Double, getY :: Double }
    deriving (Show, Eq)

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show)

instance Eq a => Eq (Tree a) where
    Leaf == Leaf = True
    Node l1 x1 r1 == Node l2 x2 r2 = l1 == l2 && x1 == x2 && r1 == r2
    _ == _ = False

contains :: Eq a => a -> Tree a -> Bool
contains _ Leaf = False
contains x (Node l y r) = x == y || contains x l || contains x r

containsBST :: Ord a => a -> Tree a -> Bool
containsBST _ Leaf = False
containsBST x (Node l y r) = case compare x y of
    LT -> containsBST x l
    EQ -> True
    GT -> containsBST x r

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l y r) = case compare x y of
    LT -> Node (insert x l) y r
    EQ -> Node l y r
    GT -> Node l y (insert x r)

delete :: Ord a => a -> Tree a -> Tree a
delete x Leaf = Leaf
delete x (Node l y r) = case compare x y of
    LT -> Node (delete x l) y r
    EQ -> Node newLeft newItem r
            where
            getAndRemoveMax (Node l2 a r2)
                | r2 == Leaf = (Node l2 a r2, a)
                | otherwise = (Node newTree a r2, a)
                    where 
                        (newTree, a) = getAndRemoveMax l2

            (newLeft, newItem) = getAndRemoveMax l

    GT -> Node l y (delete x r)



data BoolFn = BoolFn (Bool -> Bool)
instance Show BoolFn where
    show (BoolFn f) = "BoolFn (let f True = " ++ show (f True) ++ "; f False = " ++ show (f False) ++ "in f)"

instance Eq BoolFn where
    BoolFn f == BoolFn g = f True == g True && f False == g False

class HasValue a where
    hasValue :: a -> Bool

    anyValue :: [a] -> Bool
    anyValue = any hasValue

instance HasValue (Maybe a) where
    hasValue Nothing = False
    hasValue (Just _) = True


