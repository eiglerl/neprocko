{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.List
import Data.Char

search :: [String] -> [String] -> [Int]
-- Sum up the appearances for every word
search table words = sum_lists lists
    where
        -- Make sure every letter is lowered
        lower = map (map toLower)
        lower_table = lower table
        lower_words = lower words
        -- Count word appearances for every table transformation
        lists = [count_words_in_table lower_words (f lower_table) | f <- table_transformations]

count_word_in_row :: String -> String -> Int
-- Check if word is prefix of every possible suffix of row (list stores temp items)
count_word_in_row word row = length [True | i <- [0..length row - length word], word `isPrefixOf` drop i row]

-- Tests using foldr, didn't work (either incorrect or too slow)
-- count_in_row word row = foldr (\a rest -> if isPrefixOf word a then (rest+1) else (rest+0)) 0 (inits row)
-- count_word_in_row word row = foldr (\a num -> if word `isPrefixOf` a then num+1 else num+0) 0 (prefixes row)
--     where prefixes r = [drop i r | i <- [0..length r]]
-- count_word_in_row word row = fst $ foldr (\a (num,counter) -> if word `isPrefixOf` drop counter row then (num+1,counter+1) else (num+0,counter+1)) (0,0) row

count_word_in_table :: String -> [String] -> Int
-- Sum up the number of appearances for every row
-- count_word_in_table word table = sum [count_word_in_row word row + count_word_in_row (reverse word) row | row <- table]
count_word_in_table _ [] = 0
count_word_in_table word (row:rest) = count_word_in_row word row + count_word_in_row (reverse word) row + count_word_in_table word rest

count_words_in_table :: [String] -> [String] -> [Int]
-- Create a list of number of appearances for every word
-- count_words_in_table words table = [count_word_in_table word table | word <- words]
count_words_in_table words table = map count words
    where count x = count_word_in_table x table

table_transformations :: [[String] -> [String]]
-- Prepare possible table transformations
table_transformations = [id, transpose]
    where reverse_table = map reverse
-- table_transformations = [id, reverse_table, transpose, reverse_table . transpose]
--     where reverse_table = map reverse

sum_lists :: [[Int]] -> [Int]
-- A utility function to sum up the appearances for every word
sum_lists = map sum . transpose
