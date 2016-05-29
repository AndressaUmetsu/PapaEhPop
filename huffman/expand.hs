import Huffman
import Data.Char
import System.Environment
import System.IO
import System.IO.Error
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I

getReg = do
    c <- G.getWord8
    f <- G.getWord32be
    return (c,f)

getRegs n = do
    if n == 0 then return[]
        else do {r <- getReg; rs <- getRegs (n-1); return(r:rs);}

{-convertEncodedWord _ [] = ""-}
{-convertEncodedWord n (x:xs)-}
    {-| n == 0 = ""-}
    {-| n > 7 = int2bin $ ord $ I.w2c x ++ convertEncodedWord (n-8) xs-}
    {-| otherwise = take n $ int2bin $ ord $ I.w2c x-}

convertEncodedWord n xs = take n $ concat $ map (int2bin.ord.I.w2c) xs

getEncoded = do
    empty <- G.isEmpty
    if empty then return[]
        else do {r <- G.getWord8; rs <- getEncoded; return(r:rs);}

getAll = do
    numOfCharacters <- G.getWord32be
    wordLength <- G.getWord32be
    freqList <- getRegs (fromIntegral numOfCharacters)
    encodedWord <- getEncoded 
    return((fromIntegral wordLength,freqList,encodedWord))

expand tree = expand' tree
    where
        expand' (Folha a _) [] = [a]
        expand' (Folha a _) xs = a : expand' tree xs
        expand' (No _ left right) (x:xs) = expand' (if x == '0' then right else left) xs

int2bin x
    | x `div` 2 == 0 = show x
    | otherwise = int2bin (x `div` 2) ++ show (x `mod` 2) 

convert [] = []
convert ((c,f):xs) = (I.w2c c,fromIntegral f) : convert xs

main = do
    args <- getArgs
    file <- L.readFile (head args)
    let 
        (wordLength,freqList_,encodedWord_) = G.runGet getAll file
        freqList = convert freqList_
        encodedWord = convertEncodedWord wordLength encodedWord_
        leafList = huffmanList_ freqList
        tree = huffmanTree_ leafList
        expandedWord = expand (head tree) encodedWord
    writeFile "output.out" expandedWord
    return ()
