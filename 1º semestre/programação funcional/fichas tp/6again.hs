--1º
data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

--a)
altura :: BTree a -> Int
altura (Empty) = 0
altura (Node r e d) = 1 + (max (altura e) (altura d))

--b)
contaNodos :: BTree a -> Int
contaNodos (Empty) = 0
contaNodos (Node r e d) = 1 + (contaNodos e) + (contaNodos d)

--c)
folhas :: BTree a -> Int
folhas (Empty) = 0
folhas (Node x Empty Empty) = 1
folhas (Node r e d) = (folhas e) + (folhas d) 

--d)
prune :: Int -> BTree a -> BTree a
prune x Empty = Empty
prune 0 f = Empty
prune x (Node r e d) = Node r (prune (x-1) e) (prune (x-1) d)

--e)
path :: [Bool] -> BTree a -> [a]
path x Empty = []
path [] (Node r e d) = [r]
path (x:xs) (Node r e d) = if (x==True) then r:(path xs d) else r:(path xs e)

--f)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)

--g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT _ _ _ = Empty

--h)
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) e d) = (Node x e1 d1,Node y e2 d2, Node z e3 d3)
                          where (e1,e2,e3) = unzipBT e
                                (d1,d2,d3) = unzipBT d

--2ª
--a)
minimo :: Ord a => BTree a -> a
minimo (Node x Empty Empty) = x
minimo (Node r e d) = minimo e

--b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node _ Empty _) = Empty
semMinimo (Node r e d) = Node r (semMinimo e) d

--c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node r Empty _) = (r,Empty)
minSmin (Node r e d) = (a,Node r b d)
                     where (a,b) = minSmin e

--d)
remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node r e d) | x < r = Node r (remove x e) d
                      | x > r = Node r e (remove x d)
                      | otherwise = Empty

--3ª
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
                  deriving Show

type Turma = BTree Aluno

turma1 :: Turma
turma1 = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty Empty) (Node (14,"Lara",ORD,Aprov 19) Empty Empty)) (Node (20,"Pedro",TE,Aprov 10) Empty (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty Empty) (Node (28,"Vasco",MEL,Rep) Empty Empty))))

--a)
inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False
inscNum n (Node (num,_,_,_) e r) | n > num = inscNum n r
                                 | n < num = inscNum n e
                                 | otherwise = True

--b)
inscNome :: Nome -> Turma -> Bool
inscNome n Empty = False
inscNome n (Node (_,nome,_,_) e r) | n > nome = inscNome n r
                                   | n < nome = inscNome n e
                                   | otherwise = True

--c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nom,TE,_) e d) = (trabEst e) ++ [(num,nom)] ++ (trabEst d)
trabEst (Node (num,nom,reg,_) e d) = (trabEst e) ++ (trabEst d)

--d)
nota :: Numero -> Turma -> Maybe Classificacao
nota x Empty = Nothing
nota x (Node (n,_,_,clas) e d) | x == n = Just clas
                               | x < n = nota x e
                               | otherwise = nota x d

--e)
percFaltas :: Turma -> Float
percFaltas f = ((aux f) / (fromIntegral (contaNodos f))) * 100
             where aux Empty = 0
                   aux (Node (_,_,_,Faltou) e d) = 1 + aux e + aux d
                   aux (Node (_,_,_,clas) e d) = aux e + aux d

--f)
mediaAprov :: Turma -> Float
mediaAprov f = sumNot f / numNot f

sumNot :: Turma -> Float
sumNot Empty = 0
sumNot (Node (_,_,_,Aprov a) e d) = (fromIntegral a) + sumNot e + sumNot d
sumNot (Node (_,_,_,_) e d) = sumNot e + sumNot d

numNot :: Turma -> Float
numNot Empty = 0
numNot (Node (_,_,_,Aprov a) e d) = 1.0 + numNot e + numNot d
numNot (Node (_,_,_,_) e d) = numNot e + numNot d

--g)
aprovAv :: Turma -> Float
aprovAv m = let (a,b) = aux m in a / b
                      where aux Empty = (0,0)
                            aux (Node (_,_,_,Aprov a) e d) = (1 + e1 + d1,1 + e2 + d2) where (e1,e2) = aux e
                                                                                             (d1,d2) = aux d
                            aux (Node (_,_,_,Rep) e d) = (e1 + d1,1+ e2 + d2) where (e1,e2) = aux e
                                                                                    (d1,d2) = aux d
                            aux (Node (_,_,_,Faltou) e d) = (e1 + d1,e2 + d2) where (e1,e2) = aux e
                                                                                    (d1,d2) = aux d