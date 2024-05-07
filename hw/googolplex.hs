-- Base case where the exponent == 0 => (base ^ 0) % modulus == 1 
modularExponentiation _ 0 _ = 1
-- Otherwise use rule: (x*y)%z == ((x%z) * (y%z)) % z
modularExponentiation base exponent modulus = 
    -- Recursively calculate result (x ^ n) where n == exponent / 2
    let result = modularExponentiation base (div exponent 2) modulus
    in 
        -- If exponent is even then x ^ 2n == x ^ n * x ^ n
        if even exponent then
            mod (result * result) modulus
        -- else x ^ (2n+1) == x ^ n * x ^ n * x
        else
            mod (base * result * result) modulus

googolMod :: Integer -> Integer -> Integer
googolMod n k = modularExponentiation 10 (10 ^ n) k