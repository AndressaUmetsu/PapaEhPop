import Huffman
import Data.Char
import System.Environment
import System.IO
import System.IO.Error


canMakeChar :: [Char] -> Huffman -> Bool
canMakeChar _ (Folha _ _) = True
canMakeChar [] (No _ _ _) =  False
canMakeChar (x:xs) (No _ left right)
    | x == '0' = canMakeChar xs right
    | x == '1' = canMakeChar xs left

makeChar :: [Char] -> Huffman -> [Char]
makeChar _ (Folha a _) = [a]
makeChar (x:xs) (No _ left right)
    | x == '0' = makeChar xs right
    | x == '1' = makeChar xs left


expand_ :: [Char] -> [Char] -> Huffman -> [Char]
expand_  _ [] _ = ""
expand_ [] _ _ = ""
expand_ charCode (x:xs) tree
    | canMakeChar charCode tree = makeChar charCode tree ++ expand_ (head restOfTheList) (tail restOfTheList) tree
    | otherwise = expand_ (charCode ++ [x]) xs tree
    where
        restOfTheList = drop (length charCode) xs

expand :: [Char] -> Huffman -> [Char]
expand (x:xs) tree = expand_ [x] xs tree


main = do
    args <- getArgs
    file <- readFile (head args)
    let tree = read (args !! 1) :: Huffman
    writeFile (args !! 2) (expand file tree)
    return ()
