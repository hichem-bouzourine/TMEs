module Lib
    ( someFunc
    , maxInt
    , fibonacci
    ) where


someFunc :: IO ()
someFunc = putStrLn "someFunc"

maxInt :: Integer -> Integer -> Integer
maxInt n m 
    | n<m = m
    | otherwise = n

fibonacci :: Integer -> Integer
fibonacci n
    | n==0 = 0
    | n==1 = 1
    | otherwise = fibonacci(n-1) + fibonacci(n-2)
