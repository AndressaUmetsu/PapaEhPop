data Tree a = Node a (Tree a) (Tree a) | Leaf deriving (Show,Eq,Ord)

insertNode e Leaf = Node e Leaf Leaf
insertNode e a@(Node x left right)
    | e < x = Node x (insertNode e left) right
    | e > x = Node x left (insertNode e right)
    | otherwise = a

searchBiggest (Node x left right)
    | right == Leaf = x
    | otherwise = searchBiggest right

tree2list Leaf = []
tree2list (Node x Leaf Leaf) = [x]
tree2list (Node x left right)
    | left == Leaf = x:tree2list right
    | otherwise = tree2list left ++ [x] ++ tree2list right

countElements Leaf = 0
countElements (Node x left right) = 1 + countElements left + countElements right

mapTree f Leaf = Leaf
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)

{-removeNode y Leaf = Leaf-}
{-removeNode y (Node x left right) -}
    {-| y < x = Node x (removeNode y left) right-}
    {-| y > x = Node x left (removeNode y right)-}
    {-| otherwise = removeNode y (left right)-}

a = Node 5 Leaf Leaf
b = insertNode 10 a
c = insertNode 4 b
d = insertNode 12 c