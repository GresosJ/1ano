--1º
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y | x >= y = [y]
                | x < y = x:(enumFromTo' (x+1) y)

--2º
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | x < z = x:enumFromThenTo' y (y+(y-x)) z
                      | x == z   = [x]
                      | x == y   = repeat x
                      |otherwise = []

--3º
maismais :: [a] -> [a] -> [a]
maismais [] l    = l
maismais (h:t) l = h:maismais t l

--4ª
(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h
(!!!) (h:t) x = (!!!) t (x-1)

--5ª
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = (reverse' t) ++ [h]

--6º
take' :: Int -> [a] -> [a]
take' 0 l = []
take' n [] = []
take' n (h:t) = h: (take' (n-1) t)

--7º
drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' n [] = []
drop' n (h:t) = drop'(n-1) t

--8º
zip' :: [a] -> [b] -> [(a,b)]
zip' [] l = []
zip' c [] = []
zip' (a:as) (b:bs) = (a,b) : zip as bs

--9º
elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (h:t) | n == h = True
              | otherwise = elem' n t

--10º
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n b = b:replicate' (n-1) b

--11º
intersperse' :: a -> [a] -> [a]
intersperse' _ [x] = [x]
intersperse' n (x:xs) = x : n : intersperse' n xs

--12º
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:ts) = l1 : group' l2 
              where (l1,l2) = span ( == h) (h:ts) 

--13º
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

--14º
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l  = inits'(init l) ++ [l]

--15º
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = [l] ++ tails' (tail l)

--16º
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] c = True
isPrefixOf (h:t) (a:b) = if h == a then isPrefixOf t b else False

--17º
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] l2 = True
isSuffixOf l1 l2 = if (last l1) == (last l2) then isSuffixOf (init l1) (init l2) else False

--18º
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] l = True
isSubsequenceOf l [] = False
isSubsequenceOf (x:xs) (y:ys) = if x == y then isSubsequenceOf xs ys else isSubsequenceOf (x:xs) ys

--19º 
elemIndices' :: Eq a => a -> [a] -> [Int] 
elemIndices' x [] = []
elemIndices' x (h:t) | x == h = 0: l
                     | otherwise = l
                   where l = [x+1 | x <- elemIndices' x t]

--20º
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = h: nub' (aux h t)
          where aux x [] = []
                aux x (y:ys) = if x == y then aux x ys else y: aux x ys

--21º
delete' :: Eq a => a -> [a] -> [a]
delete' x [] = []
delete' x (h:t) = if x == h then t else h: delete' x t

--22º
barra :: Eq a => [a] -> [a] -> [a]
barra x [] = x
barra [] x = []
barra x (h:t) = barra (delete' h x) t
--em cima esta a delete' feita

--23º
union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' [] l = l
union' l (h:t) | elem h l = union' l t
               | otherwise = union' (l ++ [h]) t  

--24º
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' l [] = []
intersect' [] l = []
intersect' (x:xs) l | elem x l = x: intersect' l xs
                    | otherwise = intersect' l xs 

--25º 
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t) | x <= h = x:h:t
                | x > h = h: insert' x t

--26º
unwords' :: [String] -> String
unwords' [] = ""
unwords' [x] = x
unwords' (h:t) = h ++ [' '] ++ unwords' t

--27º
unlines' :: [String] -> String
unlines' [] = ""
unlines' [x] = x ++ "/n"
unlines' (h:t) = h ++ "/n" ++ unlines' t

--28º
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (h:t) | h == aux (h:t) = 0
             | otherwise = 1 + pMaior t
            where
              aux [x] = x
              aux (x:y:xs) | x>=y      = aux (x:xs)
                           | otherwise = aux (y:xs)

--29º
temRepetidos :: Eq a => [a] -> Bool
temRepetidos []    = False
temRepetidos (h:t) = elem h t || temRepetidos t

--30º
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) | (h >= '0' && h <= '9') = h:algarismos t
                 | otherwise = algarismos t

--31º
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (x:y:t) = y:posImpares t 

--32ª
posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (x:y:t) = x:posPares t

--33ª
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (h:x:t) | h > x = False
                 | otherwise = isSorted (x:t)

--34º
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert' h (iSort t)
--insert' exercicio 25º

--35ª
menor' :: String -> String -> Bool
menor' [] l = True
menor' l [] = False
menor' (x:xs) (y:ys) | x < y = True
                     | x > y = False
                     |otherwise = menor' xs ys

--36º
elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' x [] = False
elemMSet' x ((y,n):t) | x == y = True
                      | otherwise = elemMSet' x t

--37º
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((n,y):ys) = y + lengthMSet ys

--38º
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,xs):y) = aux (x,xs) ++ converteMSet y
                        where aux (x,0) = []
                              aux (x,xs) = [x] ++ (aux (x,(xs-1)))

--39º
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,xs):t) | a == x = ((x,(xs+1)):t)
                        | otherwise = (x,xs): insereMSet a t

--40º
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((x,xs):t) | a == x && xs>1 = ((x,(xs-1)):t)
                        | a == x && xs==1 = t
                        |otherwise = (x,xs): removeMSet a t 

--41º
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet l = aux 1 l
                where aux a [x] = [(x,a)]
                      aux a (x:y:z) | x == y = aux (a+1) (x:z)
                                    | x /= y = (x,a): aux 1 (y:z) 

--42º

--43º
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a :t) = a :catMaybes t
catMaybes (Nothing :t) = catMaybes t

--44º
data Movimento = Norte | Sul | Este | Oeste
                deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:t) = posicao (x,(y+1)) t
posicao (x,y) (Sul:t) = posicao (x,(y-1)) t
posicao (x,y) (Este:t) = posicao ((x+1),y) t
posicao (x,y) (Oeste:t) = posicao ((x-1),y) t

--45º
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (z,w) | x == z && y == w = []
                    | x<z = Este : caminho (x+1,y) (z,w)
                    | x>z = Oeste : caminho (x,y) (z+1,w)
                    | y<w = Norte : caminho (x,y+1) (z,w)
                    | y>w = Sul : caminho (x,y) (z,w+1)

--46º
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (Norte:t) = vertical t 
vertical (Sul:t) = vertical t
vertical (Este:t) = False 
vertical (Oeste:t) = False

--47º
data Posicao = Pos Int Int
              deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [x] = x
maisCentral (x:y:t) | aux x <= aux y = maisCentral (x:t)
                    | aux x >  aux y = maisCentral (y:t)
                    where aux (Pos a b) = sqrt( fromIntegral (a^2 + b^2))

--48º
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos x [] = []
vizinhos (Pos x y) ((Pos z w):t) = if ((y == w && x == (z+1)) || (y == w && x == (z-1)) || (y == (w+1) && x == z) || (y == (w-1) && x == z)) then (Pos z w):(vizinhos (Pos x y) t) else vizinhos (Pos x y) t

--49º
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = False
mesmaOrdenada [(Pos x y)] = True
mesmaOrdenada((Pos x y):(Pos z w):t) = if y == w then mesmaOrdenada ((Pos z w):t) else False

--50º
data Semaforo = Verde | Amarelo | Vermelho
                deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = (aux l)<=1
              where
                aux [Vermelho] = 0
                aux [Verde] = 1
                aux [Amarelo] = 1
                aux (Vermelho:t) = aux t
                aux (Verde:t) = 1+aux t
                aux (Amarelo:t) = 1+aux t