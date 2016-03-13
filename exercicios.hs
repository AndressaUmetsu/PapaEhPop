inverter[] = []
inverter(x:xs) = inverter xs ++ [x]

pali s = if inverter s == s then True else False

pertence a [] = False
pertence a (x:xs) = if a == x then True else pertence a xs

inter [] _ = []
inter (x:xs) ys = if pertence x ys then x:inter xs ys else inter xs ys
 
repete [] = False
repete (x:xs) = if pertence x xs then True else repete xs

uniao [] ys = ys
uniao (x:xs) ys = if not (pertence x ys) then x:uniao xs ys else uniao xs ys 

ultimos n xs = take n (inverter xs)

expo b 0 = 1
expo b e = b * expo b (e-1) 

converteBin [] _ = 0
converteBin (x:xs) e = if x == '1' then expo 2 e + converteBin xs (e+1) else converteBin xs (e+1)

binToint xs = converteBin (inverter(xs)) 0 

menor [x] = x 
menor (x:xs) = min x (menor xs)