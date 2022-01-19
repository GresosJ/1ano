import Data.List
import Data.Char

--1ª
data Frac = F Integer Integer

--a)
mdc :: Integer -> Integer -> Integer
mdc a b | a > b = mdc (a - b) b
        | a < b = mdc a (b - a)
        | a == b = a

normaliza :: Frac -> Frac
normaliza (F n 0) = error "denominador nulo"
normaliza (F 0 d) = F 0 1
normaliza (F n d) = F ((signum d) * (n `div` m)) ((abs d) `div` m) 
                   where m = mdc (abs n) (abs d)

--b)
instance Eq Frac where
         (==) x y = (a1==a2) && (b1==b2)
                 where (F a1 b1) = normaliza x
                       (F a2 b2) = normaliza y

--c)
instance Ord Frac where
         (<=) (F a1 b1) (F a2 b2) = (a1*b2) <= (a2*b1)

--d)
instance Show Frac where
         show (F a b) = "(" ++ show a ++ "/" ++ show b ++ ")"

--e)
instance Num Frac where
         (+) (F a1 b1) (F a2 b2) = (F (a1*b2 + a2*b1) (b1*b2))
         (-) (F a1 b1) (F a2 b2) = (F (a1*b2 - a2*b1) (b1*b2))
         (*) (F a1 b1) (F a2 b2) = (F (a1*a2) (b1*b2))
         negate (F a b) = (F (-a) b)
         abs (F a b) = (F (abs a) (abs b))
         signum (F a b) = let (F x y) = normaliza (F a b) in if (x==0) then 0 else if (x>0) then 1 else (-1)
         fromInteger x = (F x 1)

--f)
maioresFrc :: Frac -> [Frac] -> [Frac]
maioresFrc f [] = []
maioresFrc f (h:ts) = if (h > (2 * f)) then h : (maioresFrc f ts) else (maioresFrc f ts)

--2º
data Exp a = Const Int
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

calcula :: Exp a -> Int
calcula (Const x) = x
calcula (Simetrico x) = (calcula x) * (-1)
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y) = (calcula x) * (calcula y)

infixa ::Show a => Exp a -> String
infixa (Const a) = show a
infixa (Simetrico a) = "(-" ++ infixa a ++ ")"
infixa (Mais a b) = "(" ++ infixa a ++ "+" ++ infixa b ++ ")"
infixa (Menos a b) = "(" ++ infixa a ++ "-" ++ infixa b ++ ")"
infixa (Mult a b) = "(" ++ infixa a ++ "*" ++ infixa b ++ ")"

--a)
instance Show a => Show (Exp a) where
         show x = infixa x

--b)
instance Eq (Exp a) where
         (==) x y = (calcula x) == (calcula y) 

--c)
instance Num (Exp a) where
         (+) x y = (Mais x y)
         (-) x y = (Menos x y)
         (*) x y = (Mult x y)
         negate x = (Simetrico x)
         abs x = if ((calcula x) > 0) then x else (Simetrico x)
         signum x = if (calcula x == 0) then (Const 0) else if (calcula x > 0) then (Const 1) else (Const (-1))
         fromInteger x = Const (fromInteger x)

--3º
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

--a
instance Eq Data where
         (==) (D d m a) (D d1 m1 a1) = (D d m a) == (D d1 m1 a1)

instance Ord Data where
         (<=) (D d m a) (D d1 m1 a1) = (D a m d) <= (D a1 m1 d1) 

--b)
instance Show Data where
         show (D d m a) = show d ++ "/" ++ show m ++ "/" ++ show a

--c)
ordena :: Extracto -> Extracto
ordena (Ext n l) = (Ext n (sortBy (\ (data1,_,_) (data2,_,_) -> compare data1 data2) l))

--d)
instance Show Extracto where
         show (Ext n l) = "Saldo anterior" ++ show n ++
                          "---------------------------------------" ++
                          "/nData Descricao Credito Debito" ++
                          "/n---------------------------------------/n" ++
                          concatMap (\(dat,str,mov) -> show dat ++ replicate (11 - (length (show dat))) ' ' ++ map (toUpper) str ++ "    \n") l ++
                          "/n---------------------------------------" ++
                          "/nSaldo actual" ++ show (saldo (Ext n l))


saldo :: Extracto -> Float
saldo (Ext x lm) = foldl (\acc (_,_,mov) -> case mov of Credito n -> (acc + n)
                                                        Debito n -> (acc - n)) x lm

ext1 :: Extracto
ext1 = Ext 300 [(D 5 4 2010,"DEPOSITO",Credito 2000),(D 10 8 2010,"COMPRA",Debito 37.5),(D 1 9 2010,"LEV",Debito 60),(D 7 1 2011,"JUROS",Credito 100),(D 22 1 2011,"ANUIDADE",Debito 8)]
