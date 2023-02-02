module Ficha9 where

import Data.Char
import GHC.CmmToAsm.AArch64.Instr (x0)
import GHC.Parser.Lexer (xset)

--- MODAD --- IO ---
{-

IO
       
     /- getChar (ler 1 caracter)
ler <
     \-  getLine (le 1 linha) 


           /- putChar (escreve um char)
          /- putStr (escreve uma string)
escrever <
          \- putStrLn (escreve 1 string e enter)
           \- print (putStrLn + show)

- randomIO (nº random)
- randomRIO (nº aleatorio entre a e b)

-}

teste :: IO ()
teste = do print "Escreve 1 n: "
           v <- getLine
           if v == "ola"
               then print v
               else print "Es burro!"
{-
bingo :: IO ()
bingo = acumulaN []

acumulaN :: [Int] -> IO ()
acumulaN l | length l == 90 = do print "Fim do Jogo!"
           | otherwise = do 
               x <- randomRIO (1,90)
               if elem x l
                    then acumulaN l
                    else print x
               getChar 
               acumulaN (x:l)
          
partes :: String -> Char -> [String]
partes xs separador = words (map (\x -> if x == separador then ' ' else x) xs)


data FileSystem = File Nome | Dir Nome [FileSystem]
type Nome = String
fs1 = Dir "usr" [Dir "xxx" [File "abc.txt", File "readme", Dir "PF" [File "exemplo.hs"]],
     Dir "yyy" [], Dir "zzz" [Dir "tmp" [], File "teste.c"] ]

fichs :: FileSystem -> [Nome]
fichs (File x) = [x]
fichs (Dir nome resto) = concatMap fichs resto

a1 = Node 5 (Node 3 Empty Empty)
            (Node 7 Empty (Node 9 Empty Empty))

data BTree a = Empty | Node a (BTree a) (BTree a)

instance Show a => Show (BTree a) where
     show Empty = "*"
     show (Node e l r) = "(" ++ show l ++ " <-" ++ show e ++ "-> " ++ show r ++ ")"
 -}


 ---- TESTE 2021

zip1 :: [a] -> [b] -> [(a,b)]
zip1 (h:t) (x:xs) = (h,x): zip1 t xs
zip1 _ _ = []


preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:s:t)
    | s >= h = h : preCrescente (s:t)
    | otherwise = [h]