import Data.List

--1º
(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h
(!!!) (h:t) x = (!!!) t (x-1)

--2º
data Movimento = Norte | Sul | Este | Oeste
               deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:t) = posicao (x,(y+1)) t
posicao (x,y) (Sul:t) = posicao (x,(y-1)) t
posicao (x,y) (Oeste:t) = posicao ((x-1),y) t
posicao (x,y) (Este:t) = posicao ((x+1),y) t

--3º
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) | f h = True 
             | otherwise = any' f t

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
    case a of 's' -> move (x,(y-1))
              'n' -> move (x,(y+1))
              'e' -> move ((x+1),y)
              'o' -> move ((x-1),y)
              otherwise -> return (x,y)

--6º
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5), Quadrado 4, Mover (4,3) (Quadrado 2)])

--a)
vazia :: Imagem -> Bool
vazia (Quadrado a) = False
vazia (Mover (_,_) a) = vazia a
vazia (Juntar []) = True
vazia (Juntar l) = all (==True) (map vazia l)

--b)
maior :: Imagem -> Maybe Int
maior (Quadrado a) = Just a
maior (Mover (_,_) a) = maior a
maior (Juntar []) = Nothing
maior (Juntar l) = maximum' (filter (/= Nothing) (map maior l))
                where maximum' [] = Nothing
                      maximum' l = maximum l

--c)
instance Eq Imagem where
         (==) x y = (normalizaIm x (0,0)) == (normalizaIm y (0,0))

normalizaIm :: Imagem -> (Int,Int) -> Imagem
normalizaIm (Quadrado a) = 
normalizaIm (Mover (a,b) x) = 
normalizaIm (Juntar l) = 

ex1 :: Imagem
ex1 = Juntar [Mover (5,5) (Quadrado 4), Mover (5,6) (Quadrado 5), Mover (9,8) (Quadrado 2)]