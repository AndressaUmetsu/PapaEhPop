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

getAll = do
    numOfCharacters <- G.getWord32be
    wordLength <- G.getWord32be
    let freqList = getRegs numOfCharacters

expand tree = expand' tree
    where
        expand' (Folha a _) [] = [a]
        expand' (Folha a _) xs = a : expand' tree xs
        expand' (No _ left right) (x:xs) = expand' (if x == '0' then right else left) xs

main = do
    args <- getArgs
    file <- readFile (head args)
    return ()
