import Data.List


-- ID, (From, To), Capacity, [Brana]
data Letadlo = Info Int (Int, Int) Int [Brana]
data Brana = ZachytnaPlocha | Brana Int
    deriving Eq

alokace :: [Letadlo] -> [Brana] -> [(Brana, [Letadlo])]
alokace letadla brany = undefined
    -- where 

        -- sortedLetadla = sortBy (\(Info _ (from1, to1) _ _) (Info _ (from2, to2) _ _) -> compare from1 from2) letadla

allocOnePlane :: [(Brana, [Letadlo])] -> Letadlo -> [(Brana, [Letadlo])]
allocOnePlane taken (Info _ (from, to) _ brany) = undefined
    where
        possible = filter (\(b, list) -> b `elem` brany) taken



-- MATRIX
class TMatice q where
    -- soucin :: Num a => q a -> q a -> Maybe (q a)
    submatice :: q a -> (Int, Int) -> (Int, Int) -> Maybe (q a)

data RMatice a = Data (Int, Int) [((Int, Int), a)]
    deriving Show

instance TMatice RMatice where
    -- soucin (Data (x1, y1) items1) (Data (x2, y2) items2) 
    --     | not check = Nothing
    --     | otherwise = undefined

    --         where
    --             checkCorrectSizes (Data (x1, y1) _) (Data (x2, y2) _) = y1 == x2
    --             check = checkCorrectSizes (Data (x1, y1) items1) (Data (x2, y2) items2) 
    --             newTable = [ | r <- [0..x1], c <- [0..y2]]
    --             getNewValue items1 items2 r c dimension = undefined
    --                 where
    --                     rowValues = sortBy () $ filter ((== r) . fst . fst) items1
    --                     colValues = filter ((== c) . snd . fst) items2
    --                     val = sum [ | i <- [0..dimension]]
    --             filterItems (Data _ items) f = filter (f . fst) items
    --             getRow m r = filterItems m ((== r) . fst)
    --             getCol m c = filterItems m ((== c) . snd)

    --             multiplyRowCol r c = 
    
    submatice (Data (currW, currH) items) (topX, topY) (bottomX, bottomY) 
        | not correctCoords = Nothing
        | otherwise = Just (Data (w,h) filteredItems)
            where
                correctCoords = topX <= bottomX && topY <= bottomY && topX >= 0 && bottomY >= 0 && bottomX <= currW && bottomY <= currH
                filteredItems = filter (\((r, c), _) -> r >= topY && r <= bottomY && c >= topX && c <= bottomX) items
                (w, h) = (bottomX-topX, bottomY-topY)