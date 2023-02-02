module Ficha6 where
data BTree a = Empty
             | Node a (BTree a) (BTree a)
              deriving Show

---1

--a
altura :: BTree a -> Int
altura Empty = 0
altura (Node x e d) = 1 + max (altura e) (altura d)

--b
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x e d) = 1 + contaNodos e + contaNodos d

--c
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x e d) = folhas e + folhas d

--d
prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune p (Node x e d) = Node x (prune (p-1) e) (prune (p-1) d)

--e
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path (b:bs) (Node x e d)
    | b == False = x: path bs e
    | otherwise = x: path bs d

--f
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x e d) = Node x (mirror d) (mirror e)

--g
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty Empty = Empty
zipWithBT f (Node x e d ) (Node y e1 d1) = Node (f x y) (zipWithBT f e e1)  (zipWithBT f d d1) 

--h
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = (Node a unZipBTe1 unZipBTd1, Node b unZipBTe2 unZipBTd2, Node c unZipBTe3 unZipBTd3)
    where (unZipBTe1,unZipBTe2,unZipBTe3) = unzipBT e
          (unZipBTd1,unZipBTd2,unZipBTd3) = unzipBT d

---2
--a
minimo :: Ord a => BTree a -> a
minimo (Node x Empty _ ) = x
minimo (Node x e _) = minimo e

--b
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = Node x (semMinimo e) d

--c
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = (a, Node x b d)
    where (a,b) = minSmin e

--d
remove :: Ord a => a -> BTree a -> BTree a
remove = undefined


---3

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL 
                deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show
type Turma = BTree Aluno --  ́arvore bin ́aria de procura (ordenada por numero)

--a
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum x (Node (num,_,_,_) e d) = x == num || (if x < num then inscNum x e else inscNum x d)

--b
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome x (Node (_,nome,_,_) e d) = x == nome || inscNome x e || inscNome x d

--c
trabEst :: Turma -> [(Numero,Nome)]   
trabEst Empty = []
trabEst (Node (num,nome,TE,_) e d) = trabEst e ++ [(num,nome)] ++ trabEst d
trabEst (Node _ e d) = trabEst e ++ trabEst d

--d
nota :: Numero -> Turma -> Maybe Classificacao    -- CLASSIFICAO DE UM ALUNO 
nota num (Node (numm,_,_,clas) e d)
    | num == numm = Just clas
    | num < numm = nota num e
    | otherwise =  nota num d
nota _ _ = Nothing
--    | inscNum num (Node (numm,nome,reg,clas) e d) = Just clas
--    | otherwise = Nothing

--e
percFaltas :: Turma -> Float           -- % DOS ALUNOS QUE FALTARAM
percFaltas t@(Node (num,nome,reg,clas) e d) = (contaFaltas t/contaAlunos t) * 100

contaFaltas :: Turma -> Float          -- CONTA ALUNOS QUE FALTARAM
contaFaltas Empty = 0
contaFaltas (Node (num,nome,reg,Faltou) e d) = 1 + contaFaltas e + contaFaltas d
contaFaltas (Node _ e d) = contaFaltas e + contaFaltas d

contaAlunos :: Turma -> Float          -- CONTA ALUNOS
contaAlunos Empty = 0
contaAlunos (Node _ e d) = 1 + contaAlunos e + contaAlunos d

--f
mediaAprov :: Turma -> Float           -- MEDIA DOS ALUNOS QUE PASSARAM
mediaAprov t@(Node (num,nome,reg,clas) e d) = contaNotas t/contaAlunosPass t

contaNotas :: Turma -> Float           -- CONTA NOTAS DOS ALUNOS QUE PASSARAM
contaNotas Empty = 0
contaNotas (Node (_,_,_,Aprov x) e d) = fromIntegral x + contaNotas e + contaNotas d
contaNotas (Node _ e d) = contaNotas e + contaNotas d

contaAlunosPass :: Turma -> Float      -- CONTA ALUNOS QUE PASSARAM
contaAlunosPass (Node (_,_,_,Aprov x) e d) = 1 + contaAlunosPass e + contaAlunosPass d
contaAlunosPass (Node _ e d) = contaAlunosPass e + contaAlunosPass d

--g
