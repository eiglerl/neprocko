-- pyth :: Double -> Double -> Double
-- pyth a b = sqrt (a * a + b * b)

abs' :: Int -> Int
abs' x = if x < 0 then -x else x

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- >>> findPyth 10 == [(3,4,5)]
pyth :: Int -> Int -> Int
pyth a b = a * a + b * b

findPyth :: Int -> [(Int,Int,Int)]
-- findPyth x = [(a,b,floor (sqrt (fromIntegral (pyth a b)))) | a <- [1..x], b <- [1..x], a > b]
-- findPyth x = [(a,b,pyth a b) | a <- [1..x], b <- [1..x], [1..x] contains (pyth a b) ]
findPyth x = [(a,b,c) | a <- [1..x], b <- [a..x], c <- [b..x], a * a + b * b == c * c]




-- najde nejblizsi vyssi mocninu dvojky
nextPower2 :: Integer -> Integer
nextPower2 x = head (dropWhile (< x) [2^x | x <- [0..]])