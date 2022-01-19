--1ºa
perimetro :: Double -> Double
perimetro x = 2*pi*x

--1ºb
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x,y) (z,w) = sqrt ((x-z)^2+(y-w)^2)

--1ºc
primUlt :: [a] -> (a,a)
primUlt b = (head (b) , last (b))

--1ºd
multiplo :: Int -> Int -> Bool
multiplo x y = if (mod x y) == 0 then True else False

--1ºe
truncaImpar :: [a] -> [a]
truncaImpar l = if(mod (length l) 2 /= 0) then tail l else l

--1ºf
max2 :: Int -> Int -> Int
max2 x y = if x >= y then x else y

--1ªg
max3 :: Int -> Int -> Int -> Int
max3 x y z = (max2 (max2 x y) z)

--2ªa
nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c | (a == 0) || (b^2-4*a*c<0) = 0
              | (b^2-4*a*c == 0) = 1
              | (b^2-4*a*c > 0) = 2

-- 2ª IMPO.
--nRaizes :: Double -> Double -> Double -> Int
--nRaizes a b c | (a == 0) || (delta < 0) = 0
--              | (delta == 0) = 1
--              | (delta > 0) = 2
--            where delta = b^2-4*a*c

--2ªb
raizes :: Double -> Double -> Double -> [Double]
raizes a b c | nRaizes a b c == 0 = []
             | nRaizes a b c == 1 = [r]
             | nRaizes a b c == 2 = [r1,r2]
             where r = (-b) / (2*a)
                   r1 = (-b + sqrt (b^2 - 4*a*c)) / (2*a)
                   r2 = (-b - sqrt (b^2 - 4*a*c)) / (2*a)

--3ºa
{-type Hora = (Int,Int)
horas' :: Hora -> Bool
horas' (h,m) = (h >= 0) && (h < 24) && (m >= 0) && (m < 60)

--3ºb
compHoras :: Hora -> Hora -> Bool
compHoras (a,b) (x,y) = ((a < x) || ( a==x && b < y))

--3ºc
convHoras :: Hora -> Int
convHoras (x,y) = (x*60) + y

--3ªd
convHoras' :: Int -> Hora
convHoras' x = (div x 60, mod x 60)

--3ºe
difHoras :: Hora -> Hora -> Int
difHoras a b = if (compHoras a b) then (convHoras b) - (convHoras a) else (convHoras a) -(convHoras b)

--3ºf
adicionaMinutos :: Hora -> Int -> Hora
adicionaMinutos hora mins = convHoras' ((convHoras hora) + mins)
-}
data Semaforo = Verde | Amarelo | Vermelho 
               deriving (Show,Eq)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde