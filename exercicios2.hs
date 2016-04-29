import Data.List
import Data.Char

--2
numlines = zip [ 1..]
numlines xs = zip [1..] xs

--3
allnumwords [] = []
allnumwords((i,l):xs) = map (\w -> (i,w))(words l) ++ allnumwords xs
= zip (repeat i)(words l) ++ allnumwords xs

--4
sortLs xs = sortBy cmpWord xs
	where cmpword (_, w) (_, w') = compare (map toUpper w) (map toUpper w') 

--5
almalgamate [] = []
almalgamate xxs@(x:xs) = (map fst (takewhile)), snd x) : almalgamate (dropWhile eqw xs )
	where eqw (_, w') = map toUpper w' == map toUpper x

--6
makeindex txt = almalgamate (sortLs(allnumwords(lines(txt))))
makeindex = almalgamate.sortLs.allnumwords.lines