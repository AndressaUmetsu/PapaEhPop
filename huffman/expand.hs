import Huffman
import Data.Char
import System.Environment
import System.IO
import System.IO.Error


main = do
    args <- getArgs
    file <- readFile (head args)
    let tree = read (args !! 1) :: No
    writeFile (args !! 2) (compress file)
    return ()
