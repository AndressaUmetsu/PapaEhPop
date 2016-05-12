import Huffman
import Data.Char
import System.Environment
import System.IO
import System.IO.Error


canMakeChar _ (Folha _ _) = True
canMakeChar [] _ =  False
canMakeChar (x:xs) (No _ left right)
    | x == '0' = canMakeChar xs right
    | x == '1' = canMakeChar xs left
    
makeChar _ (Folha a _) = a
makeChar (x:xs) (No _ left right)
    | x == '0' = makeChar xs right
    | x == '1' = makeChar xs left


expand [] _ = []
expand xs tree = makeChar charCode tree : expand (drop (length charCode) xs) tree
    where 
        charCode = takeWhile (not . \x -> canMakeChar x tree) xs 


main = do
    args <- getArgs
    file <- readFile (head args)
    let tree = read (args !! 1) :: Huffman
    writeFile (args !! 2) (expand file tree)
    return ()
