import Text.XHtml (base)
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

class HasValue a where
    hasValue :: a -> Bool

class Collection c where
    toList :: c a -> [a]

    contains :: Eq a => a -> c a -> Bool
    contains x c = x `elem` toList c

-- instance Semigroup Int where
--     (<>) = (+)

-- instance Monoid Int where
--     mempty = 0

newtype Sum a = Sum {getSum :: a} deriving (Show)
newtype Product a = Product { getProduct :: a } deriving (Show)

instance Num a => Semigroup (Sum a) where
    Sum a <> Sum b = Sum (a+b)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

instance Num a => Semigroup (Product a) where
    Product a <> Product b = Product (a*b)

instance Num a => Monoid (Product a) where
    mempty = Product 1

instance Foldable Tree where
    foldMap f Leaf = mempty
    foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r


-- pomoci foldMap
length' :: Foldable t => t a -> Int
length' foldable = getSum $ foldMap (\_ -> Sum (1 :: Int)) foldable
-- length' fold = getSum $ foldMap (Sum (1 :: Int)) fold

sum' :: (Foldable t, Num a) => t a -> a
-- sum' = undefined
sum' foldable = getSum $ foldMap Sum foldable

product'  :: (Foldable t, Num a) => t a -> a
product' foldable = getProduct $ foldMap Product foldable

toList' :: Foldable t => t a -> [a]
toList' = foldMap (:[])

-- class Func a where
--     (.) :: a -> a -> a

-- instance Semigroup (a -> a) where
--     (<>) = (.)

-- instance Monoid (a -> a) where
--     mempty = id



foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr' f b foldable = foldMap (\x -> )