pyth :: Double -> Double -> Double
pyth a b = sqrt (a * a + b * b)

f :: Double -> Double
f = pyth 3.0

-- add :: (Num a) => a -> a -> a
-- add a b = a + b

-- mocnina funkce
-- pow 3 (+1) 4 == 7
pow :: Int -> (a -> a) -> (a -> a)
pow 1 f = f
pow n f = pow (n-1) f . f

-- aritmeticke operace pomoci pow
-- zaciname s fci succ
-- odvodime scitani, pak nasobeni, mocneni
add :: Int -> Int -> Int
add a = pow a succ

mult :: Int -> Int -> Int
mult a b = pow a (add b) 0

power :: Int -> Int -> Int
power a b = pow b (mult a) 1 

-- rozklad seznamu na podseznamy
-- [1,2,3,4] -> [([],[1,2,3,4]), ([1],[2,3,4]), ...]
-- rozklad :: [a] -> [([a],[a])]
-- rozklad [x:xs] = 
