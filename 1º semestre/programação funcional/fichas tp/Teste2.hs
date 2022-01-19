--1º
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t) | x <= h = (x:h:t)
                | x > h = h:(insert' x t) 

--2º
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a:t) = a:(catMaybes t)
catMaybes (Nothing:t) = catMaybes t

--3º
data (Exp a) = Const Int
             | Var String
             | Mais (Exp a) (Exp a)
             | Mult (Exp a) (Exp a)

{-mostra :: (Exp a) -> [Char]
mostra (Const a) =  show a
mostra (Var a) = a
mostra (Mais a b) = "(" ++ mostra a ++ "+" ++ mostra b ++")"
mostra (Mult a b) = "(" ++ mostra a ++ "*" ++ mostra b ++")" -}

instance Show (Exp a) where
       show (Const a) =  show a
       show (Var a) = a
       show (Mais a b) = "(" ++ show a ++ "+" ++ show b ++")"
       show (Mult a b) = "(" ++ show a ++ "*" ++ show b ++")"

--4ª
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = [] 
sortOn' f (h:t) = insert'' h (sortOn' f t)
                where insert'' h [] = [h]
                      insert'' h (x:y) | f h <= f x = h:x:y
                                       | otherwise = x:(insert'' h y)

--5º
--a)
amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = (last c) - (head c)
            where c = ordena l

ordena [] = []
ordena (h:t) = (insert' h (ordena t))

--b)
parte :: [Int] -> ([Int],[Int])
parte [] = ([], [])
parte (h:t) = (span {--} (ordena (h:t)))

--6º
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]
ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),
                          Quadrado 4,
                          Mover (4,3) (Quadrado 2)])

--a)
conta :: Imagem -> Int
conta (Quadrado x) = 1
conta (Mover (a,b) x) = conta x
conta (Juntar l) = sum (map conta l)

--b)
apaga :: Imagem -> IO Imagem
apaga x = do {do let c = conta x
                 d <- randomRIO (1,c)
                 a <- deleta d x
                 return a}


deleta :: Int -> Imagem -> Imagem
deleta