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

main = do
    args <- getArgs
    file <- readFile (head args)
    writeFile "huffmanTree.tree" (show$huffmanTree file)
    writeFile (args !! 1) (compress file)
    return ()
