module Stats(
    calcCaracteres,
    calcMots,
    calcLignes,
    countLetter,
    frequences,
    affichage,
    meilleurFrequence
) where

import Data.Text (Text)

import qualified Data.Text as T

import Data.List (sortBy)

calcCaracteres :: Text -> Int
calcCaracteres = T.foldl' (\x _ -> x + 1) 0 

calcMots :: Text -> Int 
calcMots = T.foldl' (\x w -> if w == ' ' then x + 1 else x) 0

calcLignes :: Text -> Int 
calcLignes = T.foldl' (\x w -> if w == '\n' then x + 1 else x) 1

countLetter :: Char -> Text -> Int 
countLetter letter = T.foldl' (\x w -> if w ==letter then x + 1 else x) 0

makeCounter :: Char -> (Text -> Int)
makeCounter = countLetter

counters :: [Text -> Int]
counters = map makeCounter ['a'..'z']

frequencies :: [Text -> Int] -> Text -> [Int]
frequencies [] _ = []
frequencies (f:fs) cs = f cs : frequencies fs cs

frequences :: Text -> [(Char, Int)]
frequences cs = zip ['a'..'z'] (frequencies counters cs)

meilleurFrequence :: [(Char, Int)] -> [(Char, Int)]
meilleurFrequence frequences = take 10 (sortBy (\(_,a) (_,b) -> compare b a) frequences)

affichage :: [(Char, Int)] -> String
affichage [] = []
affichage ((c, i):frs) = ((c:[]) ++ ": " ++ (show i) ++ "\n") ++ affichage frs