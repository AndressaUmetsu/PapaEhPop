import Huffman
import Data.Char
import System.Environment
import System.IO
import System.IO.Error

frequence x (tuple:tupleList)
    | x == fst tuple = snd tuple
    | otherwise = frequence x tupleList

compress_ [] _  = ""
compress_ (x:xs) huffCodification = freqx++compress_ xs huffCodification
    where
        freqx = frequence x huffCodification

compress xs = compress_ xs huffCodification
    where
        huffCodification = huffmanCodification xs

expand tree = expand' tree
    where
        expand' (Folha a _) [] = [a]
        expand' (Folha a _) xs = a : expand' tree xs
        expand' (No _ left right) (x:xs) = expand' (if x == '0' then right else left) xs
