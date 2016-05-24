import Huffman
import Data.Char
import System.Environment
import System.IO
import System.IO.Error
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I

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

putFreq [] = P.flush
putFreq ((c,f) : xs) = do
    P.putWord8 (I.c2w c)
    P.putWord32be (toEnum f)
    putFreq xs

putEncoded [] = P.flush
putEncoded xs
    | length xs > 8 = P.putWord8 (

putAll xs = do
    let freqList = frequenceList xs
    let encodedWord = compress xs
    P.putWord32be (toEnum (length freqList))
    P.putWord32be (toEnum (length encodedWord))
    putFreq freqList
        

main = do
    args <- getArgs
    file <- readFile (head args)
    return ()
