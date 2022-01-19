--1ºa
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

{-
funA [2,3,5,1]
2^2+(funA [3,5,1])
4+(3^2+funA[5,1])
4+9+(5^2+funA[1])
13+25+(1^2+ funA[])
38+1+0
39
-}

--1ªb
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t) else (funB t)

{- 
funB [8,5,12]
8 : (funB [5,12])
8 : (funB [12])
8 : (funB12 : (funB [])
8 : (funB12 : [])
[8,12]
-}

--1ºc
funC (x:y:t) = funC t
funC [x] = []
funC [] = []

{-
funC [1,2,3,4,5]
func[1] : (funC [2] : (funC [3,4,5]))
funC [3] : (funC [4] : (funC [5]))
funC [5]
funC []
[]
-}

--1ªd
funD l = g [] l
g l [] = l
g l (h:t) = g (h:l) t

{-
funD "otrec"
g [] "otrec"
l= [] h= 'o' t "trec"
g('o': []) "trec"
l=['o'] h='t' t="rec"
l=['t','o'] h= 'r' t="ec"
l=['r','t','o'] h= 'e' t='c'
l=['e','r','t','o'] h= 'c' t=[]
l=['c','e','r','t','o'] t=[]
"centro"
-}

--2ºa
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (h*2) : dobros t

--2ºb
numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (h:ts) = if (c == h) then 1 + numOcorre c ts else numOcorre c ts

--2ºc
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if (h>0) then positivos t else False

--2ªd
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if (h>=0) then h:(soPos t) else soPos t

--2ªe
soNeg :: [Int] -> Int
soNeg [] = 0
soNeg (h:t) = if (h<=0) then h + soNeg t else soNeg t

--2ºd
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt [a] = [a]
tresUlt [a,b] = [a,b]
tresUlt [a,b,c] = [a,b,c]
tresUlt (h:ts) = tresUlt ts

--2ºg
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):ts) = y : (segundos ts)

--2ºh
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros x [] = False
nosPrimeiros x ((a,b):t) | x == a = True
                         | otherwise = nosPrimeiros x t

--2ºi
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = st (x,y,z) (sumTriplos t)
                       where st (a,b,c) (d,e,f) = (a+d,b+e,c+f)

--3