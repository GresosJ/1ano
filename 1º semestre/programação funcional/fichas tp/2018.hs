import System.Random
import Data.List
import Data.Char
--1º
--a)
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x [] = []
elemIndices' x (h:t) | x == h = 0:l
                     | otherwise = l
                    where l = [x+1 | x <- elemIndices' x t]

--b)
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] l = True
isSubsequenceOf l [] = False
isSubsequenceOf (x:xs) (y:ys) | x == y = isSubsequenceOf xs ys
                              | otherwise = isSubsequenceOf (x:xs) ys

--2º
data Btree a = Empty | Node a (Btree a) (Btree a)
--a)
lookAp :: Ord a => a -> Btree (a,b) -> Maybe b
lookAp x Empty = Nothing
lookAp x (Node (a,b) e d) | x == a = Just b
                          | x < a = lookAp x e
                          | otherwise = lookAp x d

--b)
zipWithBT :: (a->b->c) -> Btree a -> Btree b -> Btree c
zipWithBT f Empty _ = Empty
zipWithBT f _ Empty = Empty
zipWithBT f (Node x xe xd) (Node y ye yd) = Node (f x y) (zipWithBT f xe ye) (zipWithBT f xd yd)

--3º
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) | isDigit h = (h:l1,l2)
                 | isAlpha h = (l1,h:l2)
                 | otherwise = digitAlpha t
                where (l1,l2) = digitAlpha t

isDigit h = if (h>='0' && h<='9') then True else False

isAlpha h = if (h>='a' && h<='z' || h>='A' && h<='Z') then True else False

--4º
data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)
--a)
firstSeq :: Seq a -> a
firstSeq (Cons a s) = a
firstSeq (App a s) = firstSeq a

--b)

--c)
{-instance Show (Seq a) where
            show x = mostra x

mostra :: Seq a -> String
mostra Nil = ""
mostra (Cons a Nil) = show a
mostra (Cons a s) = show a ++ "," ++ mostra s
mostra (App a s) = mostra a ++ "," ++ mostra s -}

--5º
type Mat a = [[a]]
--a)
getElem :: Mat a -> IO a
getElem l = let x = primAux l
            in do k <- randomRIO (1,x)
                  return (otaux l k)

primAux :: Mat a -> Int
primAux [] = 0
primAux (h:t) = (length h) + primAux t

otaux :: Mat a -> Int -> a
otaux ((h:t):ts) 1 = h
otaux ([]:t) = otaux t x
otaux ((h:t):ts) x = otaux (t:ts) (x-1)