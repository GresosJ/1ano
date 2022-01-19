import System.Random
import Data.List
import Data.Char

--1º
--a)
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = f h || any' f t

--b)
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f [] s = []
zipWith' f s [] = []
zipWith' f (x:xs) (y:ys) = (f x y):zipWith' f xs ys

--c)
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (h:t) | f h = h:takeWhile' f t
                   | otherwise = takeWhile' f t

--d)
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (h:t) | f h = dropWhile' f t
                   | otherwise = h:dropWhile' f t

--e)
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (h:t) | f h = (h:l1,l2)
              | otherwise = (l1,h:l2)
             where (l1,l2) = span' f t

--f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f a [] = []
deleteBy' f a (h:t) = if (f a h) then t else h:deleteBy' f a t

--g)
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (h:t) = insert h (sortOn' f t)
                where insert h [] = [h]
                      insert h (x:xs) | f h <= f x = (h:x:xs)
                                      | otherwise = x:insert h xs

--2º
type Polinomio = [Monomio]
type Monomio = (Float,Int)
p1 :: Polinomio
p1= [(2,3), (3,4), (5,3), (4,5)]
--a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau x p = filter (\ (c,e) -> e == x) p

--b)
conta :: Int -> Polinomio -> Int
conta x p = length (selgrau x p)

--c)
grau :: Polinomio -> Int
grau [(a,b)] = b
grau ((a,b):t) = max b (grau t) 

--d)
deriv :: Polinomio -> Polinomio
deriv p =map  (\ (a,b) -> (a*(fromIntegral b),b-1)) p

--e)
calcula :: Float -> Polinomio -> Float
calcula x p = foldr (\(c,e) r -> (c*x^e) + r) 0 p

--f)
simp :: Polinomio -> Polinomio
simp p = filter (\(c,e) -> c /= 0) p

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (c,e) p = map (\ (x,y) -> (c*x,y+e)) p

--h)
ordena :: Polinomio -> Polinomio
ordena p = sortOn (snd) p

--i)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = aux h (normaliza t)
               where aux (a,b) [] = [(a,b)]
                     aux (a,b) ((x,xs):y) = if (b == xs) then (a+x,b):y else (x,xs):aux (a,b) y

--j)
soma :: Polinomio -> Polinomio -> Polinomio
soma p y = normaliza ( (++) p y )

--k)
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldl (\acc x -> soma (mult x p2) acc) [] p1

--ou
produto' :: Polinomio -> Polinomio -> Polinomio
produto' [] p = p
produto' [x] p = mult x p
produto' (x:xs) p =normaliza ( (mult x p) ++ (produto xs p) )

--l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = (ordena (normaliza p1)) == (ordena (normaliza p2))

--3º
type Mat a = [[a]]

--a)
dimOK :: Mat a -> Bool
dimOK [] = False
dimOk (h:ts) = dAux (length h) ts 
             where dAux x [] = if (x > 0) then True else False
                   dAux x (h:ts) = if (x == (length h)) then dAux x ts else False

--b)
dimMat :: Mat a -> (Int,Int)
dimMat x | dimOk x = (length x, length (head x))

--c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (x:xs) (y:ys) | dimMat (x:xs) == dimMat (y:ys) = (zipWith (+) x y):addMat xs ys
                     | otherwise = []
--d)
transpose' :: Mat a -> Mat a
transpose' ([]:_) = []
transpose' m = (map (head) m):transpose' (map (tail) m)

--e)
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat [] _ = []
multMat m1 m2 = (map (\x-> sum (zipWith (*) (head m1) x)) (transpose' m2)):multMat (tail m1) m2

--