-- exe 1
inverter[] = []
inverter(x:xs) = inverter xs ++ [x]

{- exe 2 -}
pali s = if inverter s == s then True else False

-- exe 3
pertence a [] = False
pertence a (x:xs) = if a == x then True else pertence a xs

{- exe 4 -}
inter [] _ = []
inter (x:xs) ys = if pertence x ys then x:inter xs ys else inter xs ys

-- exe 5 
repete [] = False
repete (x:xs) = if pertence x xs then True else repete xs

uniao [] ys = ys
uniao (x:xs) ys = if not (pertence x ys) then x:uniao xs ys else uniao xs ys 

ultimos n xs = take n (inverter xs)

expo b 0 = 1
expo b e = b * expo b (e-1) 

--binToint
--binToint (x:xs)

menor [x] = x 
menor (x:xs) = min x (menor xs)