import Huffman
import Data.Char
import System.Environment
import System.IO
import System.IO.Error

expand tree = expand' tree
    where
        expand' (Folha a _) [] = [a]
        expand' (Folha a _) xs = a : expand' tree xs
        expand' (No _ left right) (x:xs) = expand' (if x == '0' then right else left) xs

main = do
    args <- getArgs
    file <- readFile (head args)
    --let tree = read (args !! 1) :: Huffman
    writeFile (args !! 2) (expand (read (args !! 1) :: Huffman) file)
    return ()
