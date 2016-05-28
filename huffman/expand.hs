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

getEncoded n
    | n == 0 = ""
    | n > 7 = int2bin $ ord $ I.w2c $ G.getWord8 ++ getEncoded (n-8)
    | otherwise = take n $ int2bin $ ord $ I.w2c $ G.getWord8 

getAll = do
    numOfCharacters <- G.getWord32be
    wordLength <- G.getWord32be
    let freqList = getRegs numOfCharacters
    let encodedWord = getEncoded wordLength
    return((freqList,encodedWord))

expand tree = expand' tree
    where
        expand' (Folha a _) [] = [a]
        expand' (Folha a _) xs = a : expand' tree xs
        expand' (No _ left right) (x:xs) = expand' (if x == '0' then right else left) xs

int2bin x
    | x `div` 2 == 0 = show x
    | otherwise = int2bin (x `div` 2) ++ show (x `mod` 2) 

main = do
    args <- getArgs
    file <- L.readFile (head args)
    let 
        (freqList,encodedWord) = G.runGet getAll file
        leafList = huffmanList_ freqList
        tree = huffmanTree_ leafList
        expandedWord = expand (head tree) encodedWord
    writeFile "output.out" expandedWord
    return ()
