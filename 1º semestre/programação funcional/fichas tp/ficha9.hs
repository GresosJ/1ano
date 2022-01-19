import System.Random
import Data.List

--1º
--a)
--randomRIO :: (a,a) -> IO a

bingo :: IO ()
bingo = doAux [] 
doAux l = do putStrLn "precione teclado"
             getChar -- :: IO Char
             (n,l') <- nAle l
             print n
             if (length l' == 90) then putStrLn "Terminou" else doAux l'


nAle :: [Int] -> IO (Int, [Int])
nAle l = do n <- randomRIO (1,90)
            if elem n l then nAle l else return (n, n:l)