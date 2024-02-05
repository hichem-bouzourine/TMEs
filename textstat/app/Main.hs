{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)

import qualified Data.Text as T

-- Les fonctions d'entrées sorties pour les textes
import qualified Data.Text.IO as TIO

import Stats ( calcCaracteres, calcMots, calcLignes, countLetter, frequences, affichage, meilleurFrequence)

main :: IO ()
main = do
    x <- TIO.readFile "monLivre.txt"
    TIO.putStrLn "Lecture de fichier monLivre.txt"
    putStrLn ("Nombre de caractères: " ++ show (calcCaracteres x))
    putStrLn ("Nombre de mots: " ++ show (calcMots x))
    putStrLn ("Nombre de ligne: " ++ show (calcLignes x))
    putStrLn ("Les 10 lettres les plus fréquentes:\n" ++ affichage (meilleurFrequence (frequences (T.toLower x))))