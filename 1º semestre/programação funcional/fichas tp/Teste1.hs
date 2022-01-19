--1ª
type MSet a = [(a,Int)]

--a)
cardMSet' :: MSet a -> Int
cardMSet' [] = 0
cardMSet' ((a,b):t) = b + (cardMSet' t)

--b) 

moda :: MSet a -> [a]
moda [] = []
moda ((x,y):ts) = mAux ts y [x] 
             where mAux [] _ b = b
                   mAux ((x,y):ts) a b = if (a > y) then mAux ts a b
                                              else if (a == y) then mAux ts a (b ++ [x])
                                                       else mAux ts y [x]

--c)
converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((x,y):t) = if (y>0) then x : (converteMSet ((x,(y - 1)):t)) else converteMSet t

--d)
addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies [] v n = []
addNcopies ((a,b):(x,y):t) v n | (x==v) && (y+n>b) = ((x,(y+n)):(a,b):t)
                               | (x==v) && (y+n<b) = ((a,b):(x,(y+n)):t)
                               | (a==v) && (b+n>y) = ((a,(b+n)):(x,y):t)
                               | (a==v) && (b+n<y) = ((x,y):(a,(b+n)):t)
                               | otherwise = (addNcopies t v n)

--o exercicio d esta errado mais n sei pk
--2º

data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais

--a)
instance Show SReais where
         show (AA x y) = "]" ++ show x ++ "," ++ show y ++ "["
         show (FF x y) = "[" ++ show x ++ "," ++ show y ++ "]"
         show (AF x y) = "]" ++ show x ++ "," ++ show y ++ "]"
         show (FA x y) = "[" ++ show x ++ "," ++ show y ++ "["
         show (Uniao x y) = "(" ++ show x ++ "U" ++ show y ++ ")"

--b)
pertence' :: Double -> SReais -> Bool 
pertence' z (AA x y) |z<x = False
                     | z>y = False
                     |otherwise = True 

pertence' z (FF x y) | z==x = True
                     | z==y = True
                     | z<x = False 
                     | z>y = False
                     |otherwise = True

pertence' z (AF x y) | z<=x = False
                     | z>y = False
                     |otherwise = True

pertence' z (FA x y) | z<x = False
                     | z==y = True
                     | z>y = False
                     |otherwise = True

pertence' z (Uniao x y) = (pertence' z x) || (pertence' z y) 

--c) n sei

--3º

data RTree a = R a [RTree a]

rTree1 :: RTree Int
rTree1 = R 1 [R 2 [], R 3 [R 4 [], R 6 []], R 5 [R 10 [], R 11 [R 1 [], R 2 [R 3 [R 2 []]], R 6 []]]]
rTree2 :: RTree Int 
rTree2 = R 1 [R 2 [], R 1 [], R 7 []]

--a)
--percorre :: [Int] -> RTree a -> Maybe [a]