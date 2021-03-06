import Huffman
import Data.Char
import System.Environment
import System.IO
import System.IO.Error
import Data.List
import Data.List.Split
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

make8 xs = take 8 (xs ++ repeat '0')

putEncoded [] = P.flush
putEncoded xs
    | length xs > 8 = do
                        P.putWord8 (I.c2w $ chr $ bin2int (take 8 xs))
                        putEncoded (drop 8 xs)
    | otherwise = do
                    P.putWord8 (I.c2w $ chr $ bin2int (make8 xs))

{-putEncoded [] = P.flush-}
{-putEncoded xs-}
    {-| length xs > 8 = do -}
                        {-P.putWord32be (toEnum $ bin2int (take 8 xs))-}
                        {-putEncoded (drop 8 xs)-}
    {-| otherwise = do-}
                    {-P.putWord32be (toEnum $ bin2int (make8 xs))-}

putAll xs = do
    let freqList = frequenceList xs
    let encodedWord = compress xs
    P.putWord8 (I.c2w $ chr $ length freqList)
    P.putWord8 (I.c2w $ chr $ length encodedWord)
    putFreq freqList
    putEncoded encodedWord

bin2int xs = sum [w * b | (w,b) <- zip weights (reverse bits)]
    where
        weights = iterate (*2) 1 
        bits = map digitToInt xs

joinBlocks [] = ""
joinBlocks (xs:xss) = xs ++ detToken ++ joinBlocks (xss)
    where
        detToken = if length xss /= 0 then ";" else ""

main = do
    args <- getArgs
    file_ <- readFile (head args)
    let file = reverse (drop 1 (reverse file_))    
    {-let file2 = splitOn "\n" file-}
    print file
    {-let encoded = P.runPut (putAll (joinBlocks file2))-}
    let encoded = P.runPut (putAll file)
    L.writeFile "Teste.bin" encoded
    return ()
