module Split where

import Data.List

split :: Char -> String -> [String]
split splitChar chaineDeCharactere = let tmp = dropWhile (/=splitChar) chaineDeCharactere in
    takeWhile (/=splitChar) chaineDeCharactere: if tmp == "" 
        then [] 
        else split splitChar (tail tmp)

unsplit :: Char -> [String] -> String
unsplit c s =  tail (foldl (\acc x -> acc ++ [c] ++ x ) "" s)

prop_split_unsplit :: Char -> String -> Bool
prop_split_unsplit c str = unsplit c (split c str) == str