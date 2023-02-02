-- Arvores Binarias -
{-
NOTAS:
1 - ARVORE BALANCEADA - alturas não diferem mais de uma unidade;
2 - ROSE TREES -  arvores irregulares 


-}

import System.Random

data BTree a = Empty
             | Node a (BTree a) (BTree a)
              deriving Show

--Exs
---1 (função que testa se uma função é balanceada)

testBal :: BTree a -> Bool
testBal Empty = False
testBal (Node x e d) = abs(altura e - altura d) <= 1 && testBal e && testBal d

altura :: BTree a -> Int
altura Empty = 0
altura (Node x e d) = 1 + max (altura e) (altura d)

---2 

inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node x e d) = [x] ++ inorder e ++ inorder d 


balancear :: BTree a -> BTree a
balancear t = constroi (inorder t)

constroi :: [a] -> BTree a
constroi [] = Empty
constroi l =  let n = length l
                  (l1,x:l2) = splitAt (div n 2) l
              in Node x (constroi l1) (constroi l2)


niveis :: BTree a -> [a]
niveis Empty = []
niveis (Node x e d) = x: aux [e,d] 

aux :: [BTree a] -> [a]
aux [] = []
aux (Empty:t) = aux t
aux ((Node x e d):t) = x : aux (t ++ [e,d])


data RTree a = R a [RTree a]
    deriving (Show)

contaRT :: RTree a -> Int
contaRT (R x l) = 1 + sum (map contaRT l)

alturaRT :: RTree a -> Int
alturaRT (R x []) = 1
alturaRT (R x l) = 1 + maximum (map alturaRT l) 

--preorderRT :: RTree a -> [a]
--preorderRT (R x l) = x: concatMap preoderRT l


niveisRT :: RTree a -> [a]
niveisRT t = auxRT [t]

auxRT :: [RTree a] -> [a]
auxRT [] = []
auxRT ((R x l):t) = x: auxRT (t ++ l)




data LTree a = Tip a
             | Fork (LTree a) (LTree a)
            
folhas :: LTree a -> [a]
folhas (Tip x) = [x]
folhas (Fork e d) = folhas e ++ folhas d


--------------------------------------------------------------

putStr1 :: String -> IO ()
putStr1 (x:xs) = (putChar x) >> putStr1 xs
putStr1 [] = putChar '\n'

putStr2 :: String -> IO ()
putStr2 (x:xs) = do putChar x
                    putStr xs

getLine1 :: IO String
getLine1 = getChar >>= (\x -> if x == '\n' then return [] else getLine1 >>= (\xs -> return (x:xs))) 

getLine2 :: IO String
getLine2 = do x <- getChar 
              if x == '\n'
              then return []
              else do xs <- getLine2
                      return (x:xs)

dialogo :: String -> IO String
dialogo s = do putStr s
               getLine

advinha :: IO ()
advinha = do putStr "Qual o número? "
             n <- getLine
             num <- randomRIO (1,read n)
             y <- joga num 0
             putStrLn ("Usou " ++ show y ++ " tentativas.")



joga :: Int -> Int -> IO Int
joga x n = do putStr "Indique o numero: "
              numero <- getLine
              r <- readIO numero
              if r == x then return (n+1)
              else if (r>x) 
                   then putStr "É alto..." >> joga x (n+1)
                   else do putStr "É baixo..."
                           joga x (n+1)


              