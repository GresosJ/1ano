isSorted' :: (Ord a) => [a] -> Bool
isSorted' [] = True
isSorted' (h:x:t) | h <= x = isSorted' (x:t)
                 | otherwise = False

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

maximumMB :: (Ord a) => [Maybe a] -> Maybe a
maximumMB [] = Nothing
maximumMB (Nothing:t) = maximumMB t
maximumMB (h:t) = aux h (maximumMB t)
               where aux x Nothing = x
                     aux (Just a) (Just b) | a>=b = Just a
                                           | otherwise = Just b

data LTree a = Tip a | Fork (LTree a) (LTree a)

listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork e d) = (listaLT e) ++ (listaLT d)

instance Show a => Show (LTree a) where
         show x = mostra x

mostra :: (Show a) => LTree a -> String
mostra (Tip a) = show a
mostra (Fork e d) = interspers '.' (mostra e ++ "\n" ++  mostra d)

interspers :: a -> [a] -> [a]
interspers a [x] = [x]
interspers a (h:t) = a:h:interspers a t