inverter[] = []
inverter(x:xs) = inverter xs ++ [x]

pali s = if inverter s == s then True else False

pertence a [] = False
pertence a (x:xs) = if a == x then True else pertence a xs

repete [] = False
repete (x:xs) = if pertence x xs then True else repete xs

--1
uniao [] ys = ys
uniao (x:xs) ys = if not (pertence x ys) then x:uniao xs ys else uniao xs ys 

--2
inter [] _ = []
inter (x:xs) ys = if pertence x ys then x:inter xs ys else inter xs ys

--3 
ultimos n xs = take n (inverter xs)

--4
expo b 0 = 1
expo b e = b * expo b (e-1) 

--5
converteBin [] _ = 0
converteBin (x:xs) e = if x == '1' then expo 2 e + converteBin xs (e+1) else converteBin xs (e+1)

binToint xs = converteBin (inverter(xs)) 0

--6
menor [x] = x 
menor (x:xs) = min x (menor xs)

--7
retirarElemento y (x:xs) = if x == y then xs else x:retirarElemento y xs  
	           where x = menor xs		   

ordemCres [] = []
ordemCres xs = x:ordemCres (retirarElemento x xs)

--8
insereOrd x [] = [x]
insereOrd x (y:ys) 
				| pertence x (y:ys) = y:ys
				| x < y = x:y:ys
				| otherwise = y:(insereOrd x ys)

--9
paridade x 
		| mod x 2 == 0 = True 
		| otherwise = False	

--10
_filter _[]=[]
_filter p (x:xs) = if p x then x : _filter
                 p xs else _filter p xs

--11
impares xs = _filter (not.paridade) xs

--12
primeiroTupla xs map fst xs

--13


--14
somatorio xs = fold (+) 0 xs

--16
eratosthenes :: [Int] -> [Int]
eratosthenes []    = []
eratosthenes (h:t) = h : (eratosthenes (filter (x -> mod x h /= 0) t))
-- 17
primes :: Int -> [Int]
primes x = eratosthenes [2..x]

--18
fib 1 == 1
fib 0 == 0
fib [] = fib (n-1) + fib (n-2)

--19 
-}