--1º
(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h
(!!!) (h:t) x = (!!!) t (x-1)

--2º
data Movimento = Norte 
               | Sul 
               | Este 
               | Oeste 
              deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posiçao (x,y) (Norte:t) = posiçao (x,(y+1)) t
posiçao (x,y) (Sul:t) = posiçao (x,(y-1)) t
posiçao (x,y) (Este:t) = posiçao ((x+1),y) t
posiçao (x,y) (Oeste:t) = posiçao ((x-1,y)) t

--3º
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = f h || any' f t

--4º
type Mat a = [[a]]

triSup :: (Num a, Eq a) => Mat a -> Bool
triSup [] = False
triSup m = all (==0) (takeMat 0 m)

takeMat x [] = []
takeMat 0 _ = []
takeMat x (h:t) = take x h ++ takeMat (x+1) t

--5º
movimenta :: IO (Int,Int)
movimenta = move (0,0)

move :: (Int,Int) -> IO (Int,Int)
move (x,y) = do 
    a <- getChar
    case a of 'n' -> move (x,y+1)
              's' -> move (x,y-1)
              'e' -> move (x+1,y)
              'o' -> move (x-1,y)
              otherwise -> return (x,y)
 
--6º
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

ex :: Imagem
ex = Mover (5,5)
     (Juntar [Mover (0,1) (Quadrado 5),
             Quadrado 4,
             Mover (4,3) (Quadrado 2)])

--a)
vazia :: Imagem -> Bool
vazia (Quadrado ns) = False
vazia (Mover (a,b) n) = vazia n
vazia (Juntar x) | null x = True
                 | otherwise = or (map vazia x)

--b)
maior :: Imagem -> Maybe Int
maior (Quadrado n) = Just n
maior (Mover _ n) = maior n
maior (Juntar n) | null n = Nothing
                 | otherwise = maximum' (filter (/= Nothing) (map maior n))
                 where maximum' [] = Nothing
                       maximum' l = maximum l

--c)
