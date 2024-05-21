import Data.List


prekomprese :: String -> [(Int, Int, Char)]
prekomprese = prekomprese' []

prekomprese' :: String -> String -> [(Int, Int, Char)]
prekomprese' left [] = []
prekomprese' left right = (dist, len, char) : prekomprese' (left ++ dif) (drop (len+1) right)
    where
        (dist, len, char) = longestPrefix left right
        dif = take (len +1) right


longestPrefix :: String -> String -> (Int, Int, Char)
longestPrefix left right = if null prefixes then (0,0, head right) else (revStart + len, len, right !! len)
    where
        revLeft = reverse left
        prefixes = takeWhile (\(check,_,_) -> check) [isSubstring revLeft (reverse init) 0 | init <- inits right]
        (check, revStart, len) = last prefixes



isSubstring :: String -> String -> Int -> (Bool, Int, Int)
isSutstring _ [] _ = (True, 0, 0)
isSubstring [] _ _ = (False, 0, 0)
isSubstring str sub start = if check then (True, start, length sub) else isSubstring (tail str) sub (start+1)
    where
        check = take (length sub) str == sub