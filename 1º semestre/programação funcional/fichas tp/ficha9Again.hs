import System.Random
import Data.List
import Data.Char

--1ª
--a)
bingo :: IO ()
bingo = auxBingo []

auxBingo :: [Int] -> IO ()
auxBingo l = if (length l == 90 ) then print ("ja sairam todas" ++ show l)
                                  else do getChar
                                          x <- randomRIO (1,90)
                                          if elem x l then auxBingo l
                                                      else do {print (x:l); auxBingo (x:l)} 

--b)
mastermind :: IO ()
mastermind = do <- fazerChave
             

fazerChave :: IO (Int,Int,Int,Int)
fazerChave = do {do x <- randomRIO (0,9)
                   y <- randomRIO (0,9)
                   z <- randomRIO (0,9)
                   w <- randomRIO (0,9)
                   return (x,y,z,w)
               }