module Huffman where

import Data.List

data Huffman = Folha Char Int | No Int Huffman Huffman deriving (Show, Eq, Read)

instance Ord Huffman where
    
    compare huffman1 huffman2
        | x > y = GT
        | x < y = LT
        | otherwise = EQ
            where
                x = getFrequence huffman1
                y = getFrequence huffman2

getFrequence (Folha _ x) = x
getFrequence (No y _ _) = y

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

huffmanTree_ [singleElement] = [singleElement]
huffmanTree_ (huffman1:huffman2:queue) = huffmanTree_ (insert newNode queue)
    where
        x = getFrequence huffman1
        y = getFrequence huffman2
        newNode = No (x+y) huffman1 huffman2

huffmanTree xs = head huffList
    where
        huffList = huffmanTree_ (huffmanList xs)

huffmanCodification_ freq leaf@(Folha c _) = [(c,freq)]
huffmanCodification_ freq node@(No _ left right) = huffmanCodification_ (freq ++ "1") left++huffmanCodification_ (freq ++ "0") right 

huffmanCodification xs = huffmanCodification_ "1" left++huffmanCodification_ "0" right
    where
        (No _ left right) = huffmanTree xs
