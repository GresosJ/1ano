--1ª
--a)
unlines' :: [String] -> String
unlines' [] = ""
unlines' [x] = x
unlines' (h:t) = h ++ "/n" ++ unlines' t

--b)
(//):: Eq a => [a] -> [a] -> [a]
(//) [] x = []
(//) x [] = x
(//) x (h:t) = (//) (delete h x) t

delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (h:t) = if x == h then t else h:delete x t

--2ª
data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a

--a)
primeiro :: Seq a -> a
primeiro (Inicio a s) = a
primeiro (Fim Nil a) = a
primeiro (Fim s a) = primeiro s

--b)
semUltimo :: Seq a -> Seq a
semUltimo (Inicio a Nil) = Nil
semUltimo (Inicio a s) = semUltimo s
semUltimo (Fim s a) = s

--3ª
data BTree a = Empty | Node a (BTree a) (BTree a)

--a)
prune :: Int -> BTree a -> BTree a
prune 0 b = Empty
prune x (Node r e d) = Node r (prune (x-1) e) (prune (x-1) d)

--b)
semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = Node r (semMinimo e) d

--4ª
type Tabuleiro = [String]
exemplo = ["..R.",
           "R...",
           "...R",
           ".R.."]

--a)
posicoes :: Tabuleiro -> [(Int,Int)]
posicoes [] = []
posicoes (h:t) = ((elemIndices 'R' h),(elemIndices h l)):posicoes t
              where l = (h:t)

elemIndices :: Eq a => a -> [a] -> Int
elemIndices x [] = error "nao existe nenhum elemento igual"
elemIndices x (h:t) | x == h = 0
                    |otherwise = elemIndices x t