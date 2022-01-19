--1º
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
        (==) x y = (a1 == a2) && (b1 == b2) where
                 (F a1 b1) = normaliza x
                 (F a2 b2) = normaliza y

--c)
instance Ord Frac where
         (<=) (F a1 b1) (F a2 b2) = (a1*b2) <= (a2*b1)
         -- (a1/b1) < (a2/b2) (=)
         -- (=) (a1*b2) < (a2*b1)

--d)
instance Show Frac where
         show (F a b) = "(" ++ show a ++ "/" ++ show b ++ ")"

--e)
instance Num Frac where
         (+) (F a1 b1) (F a2 b2) = normaliza (F (a1*b2 + a2*b1) (b1*b2))
         (-) (F a1 b1) (F a2 b2) = normaliza (F (a1*b2 - a2*b1) (b1*b2))
         (*) (F a1 b1) (F a2 b2) = normaliza (F (a1*a2) (b1*b2))
         negate (F a b) = (F (-a) b)
         abs (F a b) = (F (abs a) (abs b))
         signum (F n d) = let (F a b) = normaliza (F n d) in if (a == 0) then 0 else if (a>0) then 1 else (-1)
         fromInteger x = (F x 1)

--f)
maioresFrc :: Frac -> [Frac] -> [Frac]
maioresFrc f [] = []
maioresFrc f (h:ts) = if (h > (2 * f)) then h : (maioresFrc f ts) else (maioresFrc f ts)

--2ª
data ExpInt = Const a
           | Simetrico (ExpInt)
           | Mais (ExpInt) (ExpInt)
           | Menos (ExpInt) (ExpInt)
           | Mult (ExpInt) (ExpInt)

infixa :: Show a => (ExpInt) -> String
infixa (Const x) = show x
infixa (Simetrico x) = "(-" ++ (infixa x) ++ ")"
infixa (Mais x y) = "(" ++ (infixa x) ++ "+" ++ (infixa y) ++ ")"
infixa (Menos x y) = "(" ++ (infixa x) ++ "-" ++ (infixa y) ++ ")"
infixa (Mult x y) = "(" ++ (infixa x) ++ "*" ++ (infixa y) ++ ")"

calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico x) = (calcula x) * (-1)
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y) = (calcula x) * (calcula y)

posfixa :: ExpInt -> String
posfixa (Const n) = show n
posfixa (Simetrico e) = posfixa e ++ " n"
posfixa (Mais a b) = posfixa a ++ " " ++ posfixa b ++ " +"
posfixa (Menos a b) = posfixa a ++ " " ++ posfixa b ++ " -"
posfixa (Mult a b) = posfixa a ++ " " ++ posfixa b ++ " *"

--a)
instance Show (ExpInt) where
         show a = infixa a

--b)
instance Eq (ExpInt) where
         (==) x y = (calcula x) == (calcula y)

--c)
instance Num (ExpInt) where
         (+) x y = (Mais x y)
         (-) x y = (Menos x y)
         (*) x y = (Mult x y)
         negate x = (Simetrico x)
         abs x = if ((calcula x) < 0) then (Simetrico x) else x
         signum x = if (calcula x == 0) then (Const 0) else if (calcula x > 0) then (Const 1) else (Const (-1)) 
         fromInteger x = Const (fromInteger x)