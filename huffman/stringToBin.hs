import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I

escrita = do
    txt <- readFile "Teste.txt"
    let xs = freq txt
    let bs = P.runPut (put xs)
    L.writeFile "Teste.bin" bs

freq [] = []
freq (x:xs) = (x,length (filter (==x) xs) + 1) : freq (filter (/= x) xs)


put [] = P.flush
put ((c,f) : xs) = do
    P.putWord8 (I.c2w c)
    P.putWord32be (toEnum f)
    put xs
