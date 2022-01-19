--1º
--a)
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = f h || any' f t

--b)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

--c)
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' t [] = []
takeWhile' t (x:xs) | t x = x:(takeWhile' t xs)
                    | otherwise = []

--d)
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (h:t) | f h = dropWhile' f t
                   | otherwise = (h:t)

--e)
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (h:t) = if f h then (h:a,b) else ([], (h:t))
              where (a,b) = span' f t 

--f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f x [] = []
deleteBy' f x (y:ys) = if x `f` y then ys else y : deleteBy' f x ys

--g)
--sortOn :: Ord b => (a -> b) -> [a] -> [a]

--2º
type Polinomio = [Monomio]
type Monomio = (Float,Int)

p1 :: Polinomio
p1= [(2,3), (3,4), (5,3), (4,5)]

--a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau g poli = filter (\(c,e) -> e == g) poli

--d)
deriv :: Polinomio -> Polinomio
deriv poli = map (\(c,e) -> (c * (fromIntegral e),e - 1)) poli

--e)
calcula :: Float -> Polinomio -> Float
calcula x p = foldr (\(c,e) r -> (c*x^e) + r) 0 p
--calcula x [] = 0
--calcula x ((a,b):t) = ((x^b) * a) + calcula x t 

--f)
simp :: Polinomio -> Polinomio
simp l  = filter (\(c,e) -> c /= 0) l

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) [] = []
mult (c,e) ((a,b):t) = ((c * a),(e + b)) : mult (c,e) t

--3
type Mat a = [[a]]

m1 :: Mat Int
m1 = [[1,2,3], [0,4,5], [0,0,6]]

--a)
dimOk :: Mat a -> Bool
dimOk [] = False
dimOk (h:ts) = dAux (length h) ts 
             where dAux x [] = if (x > 0) then True else False
                   dAux x (h:ts) = if (x == (length h)) then dAux x ts else False

--b)
dimMat :: Mat a -> (Int,Int)
dimMat x | dimOk x = (length x , length (head x))

--c)
