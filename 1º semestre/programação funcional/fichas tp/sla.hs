import System.Random


type MSet a = [(a,Int)]


data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)



data SReais = AA Double Double 
             | FF Double Double
             | AF Double Double 
             | FA Double Double
             | Uniao SReais SReais




--{17/18 : 1}                         
insert' :: Ord a => a -> [a] ->[a] 
insert' x [] = [x]
insert' x (h:t) | x <= h = (x:(h:t))
                | x > h = (h:(insert' x t))



--{17/18 : 2}
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just x):t) = (x : catMaybes' t) 
catMaybes' ((Nothing):t) = catMaybes' t



-- {17/18 : 3 }
instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Var x) = x
    show (Mais x  y) = "(" ++ show x ++ " + " ++ show y ++ ")"    
    show (Mult x y) = "(" ++ show x ++ " * " ++ show y ++ ")" 




-- {17/18 : 4}
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (x:[]) = [x]
sortOn' f (x:t) = aux f x (sortOn' f t)
                where aux f x [] = [x]
                      aux f x (h:t) | (f x) <= (f h) = (x:h:t)
                                    | (f x) > (f h) = (h:(aux f x t))




-- {17/18 : 5 : a}
amplitude :: [Int] -> Int 
amplitude [] = 0 
amplitude (h:t) = let (x,xs) = foldr (\ a (b,c) -> if a < b then (a,c) else if a > c then (b,a) else (b,c)) (h,h) t    
                  in (xs - x)



--{17/18 : 6 : a}
conta :: Imagem -> Int
conta (Quadrado x) = 1
conta (Mover (_,_) x) = conta x
conta (Juntar t) = sum (map conta t)

{--
--{17/18 : 6 : b}
apaga :: Imagem -> IO Imagem
apaga x = do {do let c = conta x
                 d <- randomRIO (1,c)
                 --insira uma função para apagar o quadrado de numero "d"
                 return {--resultado da funão de apagar o quadrado--}}
--}



-- {16/17 : 1 : a}

cardMSet :: MSet a -> Int
cardMSet p = foldr (\ (_,c) u -> c + u) 0 p



--{16/17 : 1 : b}

moda :: MSet a -> [a]
moda ((h,b):(g,i):t)|b == i = (h:moda ((g,i):t))
                    | otherwise = [h] 



--{16/17 : 1 : c}
converteMSet :: MSet a -> [a]
converteMSet p = foldr (\ (x,y) q -> (replicate y x) ++ q) [] p      



--{16/17 : 1 : d}

addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies p x y = let ((a,g),b) = getFromMset x p
                   in insereInMset (a,(g+y)) b  



getFromMset :: Eq a => a -> MSet a -> ((a,Int),MSet a)
getFromMset x p = foldr (\ (q,r) ((s,p),t) -> if q == s then ((q,r),t) else ((s,p),((q,r):t))) ((x,0),[]) p 



insereInMset :: (a,Int) -> MSet a -> MSet a
insereInMset (x,y) [] = [(x,y)]
insereInMset (x,y) ((g,h):t) | y > h = ((x,y):(g,h):t)
                             | otherwise = ((g,h): (insereInMset (x,y) t))

--{16/17 : 2 : a}

instance Show SReais where
      show (AA x y) = "]" ++ show x ++ "," ++ show y ++ "["
      show (AF x y) = "]" ++ show x ++ "," ++ show y ++ "]"  
      show (FF x y) = "[" ++ show x ++ "," ++ show y ++ "]"
      show (FA x y) = "[" ++ show x ++ "," ++ show y ++ "["
      show (Uniao x y) = "(" ++ show x ++ " U " ++ show y ++ ")" 



--tem mais mas n estou c tempo se tiverem dúvidas nas outras me mandem 