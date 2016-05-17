import Huffman
import Data.Char
import System.Environment
import System.IO
import System.IO.Error


--canMakeChar :: [Char] -> Huffman -> Bool
canMakeChar _ (Folha _ _) = True
canMakeChar [] (No _ _ _) =  False
canMakeChar (x:xs) (No _ left right)
    | x == '0' = canMakeChar xs right
    | x == '1' = canMakeChar xs left

--makeChar :: [Char] -> Huffman -> [Char]
makeChar _ (Folha a _) = [a]
makeChar (x:xs) (No _ left right)
    | x == '0' = makeChar xs right
    | x == '1' = makeChar xs left

--expand_ :: [Char] -> [Char] -> Huffman -> [Char]
expand tree = expand' tree
    where
        expand' (Folha a _) [] = [a]
        expand' (Folha a _) xs = a : expand' tree xs
        expand' (No _ left right) (x:xs) = expand' (if x == '0' then right else left) xs

main = do
    args <- getArgs
    file <- readFile (head args)
    let tree = read (args !! 1) :: Huffman
    writeFile (args !! 2) (expand tree file)
    return ()
