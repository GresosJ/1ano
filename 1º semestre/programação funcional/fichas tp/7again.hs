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
infixa (Const a) = show a
infixa (Simetrico a) = "(-" ++ infixa a ++ ")"
infixa (Mais a b) = "(" ++ infixa a ++ "+" ++ infixa b ++ ")"
infixa (Menos a b) = "(" ++ infixa a ++ "-" ++ infixa b ++ ")"
infixa (Mult a b) = "(" ++ infixa a ++ "*" ++ infixa b ++ ")"

--c)
posfixa :: ExpInt -> String
posfixa (Const n) = show n
posfixa (Simetrico e) = posfixa e ++ " n"
posfixa (Mais a b) = posfixa a ++ " " ++ posfixa b ++ " +"
posfixa (Menos a b) = posfixa a ++ " " ++ posfixa b ++ " -"
posfixa (Mult a b) = posfixa a ++ " " ++ posfixa b ++ " *"

--2º
data RTree a = R a [RTree a]
             deriving Show
--a)
soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R a l) = a + (sum (map soma l))

--b)
altura :: RTree a -> Int
altura (R e d) = 1 + (foldl max 0 (map altura d))

--c)
prune' :: Int -> RTree a -> RTree a
prune' x (R e []) = R e []
prune' 1 (R e d) = R e []
prune' x (R e d) = R e (map (prune' (x-1)) d)

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
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty,Tip b)
splitFTree (No r e d) = (Node r l1 r1, Fork l2 r2)
                     where (l1,l2) = splitFTree e
                           (r1,r2) = splitFTree d

--b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty _ = Nothing
joinTrees _ (Tip a)= Just (Leaf a)
joinTrees (Node r e1 d1) (Fork e2 d2) = let p = joinTrees e1 e2
                                            k = joinTrees d1 d2
                                           in case p of
                                                   Nothing -> Nothing
                                                   (Just y) -> case k of
                                                                    Nothing -> Nothing
                                                                    (Just z) -> Just (No r y z)