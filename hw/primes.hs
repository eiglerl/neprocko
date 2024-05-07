-- Find prime factors of X
factors :: Int -> [Int]
-- using helper function
factors x = factorsHelper x x 2

-- Recursive helper function to find factors
factorsHelper :: Int -> Int -> Int -> [Int]
-- base case when x == n => x/n == 1
factorsHelper original 1 n = []
-- Try '2' as many times as possible 
factorsHelper original x 2 = if mod x 2 == 0 then 2 : factorsHelper original (div x 2) 2 else factorsHelper original x 3
-- Then continue with odd numbers
factorsHelper original x n = 
    -- If x % n == 0
    if mod x n == 0 then
        -- Add n and continue with x/n
        n : factorsHelper original (div x n) n

    else 
        (
        -- If n <= sqrt original
        if n * n <= original then
            -- then continue with larger n (+2 becase we only need to check odd numbers)
            factorsHelper original x (n+2) 
        -- otherwise add x
        else
            [x]
        )

-- Find largest prime gap in the range 2 .. x
gap :: Int -> (Int,Int)
gap x = maxGap (consecutivePairs (findPrimes 2 x))

-- Function to find all prime numbers in the range min .. max
findPrimes :: Int -> Int -> [Int]
-- Prime checking is done via the factors function and checking if the number has only itself as a prime factor.
findPrimes min max = [prime | prime <- [min..max], factors prime == [prime]]

-- Create a list of consecutive pairs
consecutivePairs :: [Int] -> [(Int, Int)]
consecutivePairs x = zip x (tail x)

-- Binary function that gets 2 gaps and returns the larger one
maxPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxPair (a,b) (c,d) = if b - a > d - c then (a,b) else (c,d)

-- Find max gap in list of pairs
maxGap :: [(Int, Int)] -> (Int, Int)
-- By calling maxPair on every pair
maxGap pairs = foldr maxPair (0,0) pairs