--1º
--a)
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "/n" ++ unlines' t

--b)
barra :: (Eq a) => [a] -> [a] -> [a]
barra x [] = x
barra [] x = []
barra x (h:t) = barra (delete' h x) t

delete' x [] = []
delete' x (h:t) = if x == h then t else h: delete' x t

--2ª
data Seq a = Nil 
           | Inicio a (Seq a) 
           | Fim (Seq a) a

--a)
primeiro :: Seq a -> a
primeiro (Inicio a s) = a
primeiro (Fim Nil a) = a
primeiro (Fim s a) = primeiro s

--b)
semUltimo :: Seq a -> Seq a
semUltimo (Inicio a Nil) = Nil
semUltimo (Inicio a s) = Inicio a (semUltimo s)
semUltimo (Fim s a) = s

--3º
data BTree a = Empty 
             | Node a (BTree a) (BTree a)

--a)
prune :: Int -> BTree a -> BTree a
prune x Empty = Empty
prune 0 f = Empty
prune x (Node a y z) = Node a (prune (x - 1) y) (prune (x - 1) z)

--b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = Node r (semMinimo e) d

--4º
type Tabuleiro = [String]
exemplo :: Tabuleiro
exemplo = ["..R.",
           "R...",
           "...R",
           ".R.."]

--a)
posicoes :: Tabuleiro -> [(Int,Int)]
posicoes [] = []
posicoes l = aux l 0 0

aux :: Tabuleiro -> Int -> Int -> [(Int,Int)]
aux [] _ _ = []
aux (h:t) x y = (posicaoAux h x y) ++ aux t x (y+1)

posicaoAux :: String -> Int -> Int -> [(Int,Int)]
posicaoAux [] _ _ = []
posicaoAux (h:t) x y | h == 'R' = (x,y) : posicaoAux t (x+1) y
                     | otherwise = posicaoAux t (x+1) y 

--b)
valido :: Tabuleiro -> Bool
valido [] = False
valido l = verifica (posicoes l)

verifica :: [(Int,Int)] -> Bool
verifica [] = True
verifica [x] = True
verifica ((x,y):t) | elem y (map (snd) t) || elem x (map (fst) t) || elem (x-y) (zipWith (-) (map (fst) t) (map (snd) t)) = False
                   | otherwise = verifica t

--c)
bemFormado :: Int -> Tabuleiro -> Bool
bemFormado x [] = False
bemFormado x (h:t) | all (==True) (map (\l -> x==length l) (h:t)) && length (h:t) == x && length (concat (map (takeWhile' (=='R')) (h:t))) == x = True
                   | otherwise = False

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (h:t) | f h = h: takeWhile' f t
                   | otherwise = takeWhile' f t