module Huffman where

import Data.List

data Huffman = Folha Char Int | No Int Huffman Huffman deriving (Show, Eq)

instance Ord Huffman where
    compare (Folha _ x) (Folha _ y)
        | x > y = GT
        | x < y = LT
        | otherwise = EQ

    compare (Folha _ x) (No y _ _)
        | x > y = GT
        | x < y = LT
        | otherwise = EQ 

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
frequenceList xs = sortBy compareTuples $ zip characters (listFrequenceCalculator characters xs)
    where
        characters = removeDups xs 

huffmanList_ [] = []
huffmanList_ (tuple:tupleList) = Folha (fst tuple) (snd tuple) : huffmanList_ tupleList

huffmanList xs = huffmanList_ tupleList
    where
        tupleList = frequenceList xs

--huffmanTree (leaf:leafList) 
