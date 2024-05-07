{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- Relation R is equivalence relation iff 
-- R is reflexive && R is symmetric && R is transitive
-- on a set A
is_equiv :: (a -> a -> Bool) -> [a] -> Bool
is_equiv f list = is_reflexive f list && is_symmetric f list && is_transitive f list

-- Relation R is reflective if
-- xRx forall x in A
is_reflexive :: (a -> a -> Bool) -> [a] -> Bool
-- check xRx (f x x) forall x in list
is_reflexive f list = and [f x x | x <- list]

-- Relation R is symmetric if
-- xRy => yRx forall x,y in A
is_symmetric :: (a -> a -> Bool) -> [a] -> Bool
is_symmetric f list = and [check f a b | a <- list, b <- list]
    -- The implication holds if either
        -- not xRy (not (f x y))
        -- or xRy and yRx (f x y && f y x)
    where check function x y = not (function x y) || function y x

-- Relation R is transitive if
-- xRy && yRz => xRz forall x,y,z in A
is_transitive :: (a -> a -> Bool) -> [a] -> Bool
is_transitive f list = and [check f a b c | a <- list, b <- list, c <- list]
    -- The implication holds if either
        -- not xRy (f x y)
        -- or xRy and not yRz (f x y && not (f y z))
        -- or xRy and yRz and xRz (f x y && f y z && f x z)
    where check function x y z = not (function x y) || not (function y z) || function x z

-- Find all equivalence classes given an equivalence
classes :: (a -> a -> Bool) -> [a] -> [[a]]
classes _ [] = []
classes f (x:xs) =
    let
        -- Start by finding every equivalent item to x
        eq = filter (f x) (x:xs)
        -- Find the rest
        rest = filter (not . f x) (x:xs)
    in
        -- Add equivalence class containing x and recursivelly continue on rest
        eq : classes f rest


reflexive_closure :: Eq a => (a -> a -> Bool) -> (a -> a -> Bool)
-- Return a relation that hold if either 
    -- f holds
    -- or x == y 
reflexive_closure f = q 
    where q x y = f x y || x == y