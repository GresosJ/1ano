--1.
data BTree a = Empty
             | Node a (BTree a) (BTree a)
               deriving Show

--a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node x y z) = 1 + (max (altura y) (altura z))

--b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x y z) = 1 + (contaNodos y) + (contaNodos z)

--c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x y z) = (folhas y) + (folhas z)

--d)
prune :: Int -> BTree a -> BTree a
prune x Empty = Empty
prune 0 f = Empty
prune x (Node a y z) = Node a (prune (x - 1) y) (prune (x - 1) z)

--prune 1 (Node 5 (Node 2 (Node 3 (Node 4 Empty Empty) Empty) Empty) (Node 1 Empty Empty)) 

--e)
path :: [Bool] -> BTree a -> [a]
path x Empty = []
path [] (Node a b c) = [a]
path (x:xs) (Node a b c) = if (x == True) then a :(path xs c) else a :(path xs b) 

--f)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x l r) = Node x (mirror r) (mirror l)

--g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x xl xr) (Node y yl yr) = Node (f x y) (zipWithBT f xl yl) (zipWithBT f xr yr)
zipWithBT _ _ _ = Empty 

--h)
unzipBT :: (BTree (a,b,c)) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) l r) = (Node x al ar, Node y bl br, Node z cl cr) 
                           where (al, bl, cl) = unzipBT l
                                 (ar, br, cr) = unzipBT r

--2ª
--a)
minimo :: Ord a => BTree a -> a
minimo (Node x Empty Empty) = x
minimo (Node x y z) = minimo y

--b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node r e d) = Node r (semMinimo e) d