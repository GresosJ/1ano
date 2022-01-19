--1º
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
--a)
calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico x) = (calcula x) * (-1)
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y) = (calcula x) * (calcula y)

--b)
infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico x) = "(-" ++ (infixa x) ++ ")"
infixa (Mais x y) = "(" ++ (infixa x) ++ "+" ++ (infixa y) ++ ")"
infixa (Menos x y) = "(" ++ (infixa x) ++ "-" ++ (infixa y) ++ ")"
infixa (Mult x y) = "(" ++ (infixa x) ++ "*" ++ (infixa y) ++ ")"

--c)
posfixa :: ExpInt -> String
posfixa (Const n) = show n
posfixa (Simetrico e) = posfixa e ++ " n"
posfixa (Mais a b) = posfixa a ++ " " ++ posfixa b ++ " +"
posfixa (Menos a b) = posfixa a ++ " " ++ posfixa b ++ " -"
posfixa (Mult a b) = posfixa a ++ " " ++ posfixa b ++ " *"

--2ª
data RTree a = R a [RTree a]
               deriving Show

--rose = (R 1 [R 2 [], R 3 [R 4 []], R 2 []])
--a)
soma :: (Num a) => (RTree a) -> a
soma (R a []) = a
soma (R a l) = a + (sum (map soma l))

--b)
altura :: RTree a -> Int
altura (R a l) = 1 + (foldl max 0 (map altura l))

--c)
prune :: Int -> RTree a -> RTree a
prune 1 (R e d) = R e []
prune x (R e d) = R e (map (prune(x-1)) d)

--d)
mirror :: RTree a -> RTree a
mirror (R e d) = R e (reverse (map mirror d))

--e)
postorder :: RTree a -> [a]
postorder (R e d) = foldr (++) [e] (map postorder d)

--3º
data BTree a = Empty | Node a (BTree a) (BTree a)
               deriving Show

data LTree a = Tip a | Fork (LTree a) (LTree a)
               deriving Show

--leaf = (Fork (Tip 3) (Tip 4))

--a)
ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork a b) = (ltSum a) + (ltSum b)

--b)
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork a b) = (listaLT a) ++ (listaLT b)

--c)
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork a b) = (max (ltHeight a) (ltHeight b))

--4º
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
                 deriving Show

--a)
splitFTree :: (FTree a b) -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty,Tip x)
splitFTree (No x l r) = let (l1, l2) = splitFTree l
                            (r1, r2) = splitFTree r
                        in (Node x l1 r1, Fork l2 r2)
