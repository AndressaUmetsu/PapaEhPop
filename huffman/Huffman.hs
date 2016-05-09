module Huffman where

import Data.List

data Huffman = Folha Char Int | No Int Huffman Huffman

compareTuples a b
    | snd a < snd b = LT
    | otherwise = GT

frequenceCalculator _ [] = 0
frequenceCalculator y (x:xs)
    | y == x = 1 + frequenceCalculator y xs
    | otherwise = frequenceCalculator y xs

listFrequenceCalculator [] _ = []
listFrequenceCalculator (y:ys) xs = frequenceCalculator y xs : listFrequenceCalculator ys xs

removeDups [] = []
removeDups (x:xs) 
    | x `elem` xs   = removeDups xs
    | otherwise     = x : removeDups xs

--frequenceList [] = []
frequenceList xs = zip characters (listFrequenceCalculator characters xs)
    where
        characters = removeDups xs 

