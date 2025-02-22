module Revrev where

-- reverse :: [a] -> [a]

prop_revrev :: Eq a => [a] -> Bool
prop_revrev xs = reverse (reverse xs) == xs

prop_revapp :: Eq a => [a] -> [a] -> Bool
prop_revapp xs ys = reverse (xs <> ys) == reverse ys <> reverse xs -- reverse ([1,2,3]:[4,5,6]) = reverse ([4,5,6]) : reverse ([1,2,3])

                  

                      
