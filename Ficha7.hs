module Ficha7 where

data ExpInt = Const Int
    | Simetrico ExpInt
    | Mais ExpInt ExpInt
    | Menos ExpInt ExpInt
    | Mult ExpInt ExpInt

---1
calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico x) = -1 * calcula x 
calcula (Mais x y) = calcula x + calcula y
calcula (Menos x y) = calcula x - calcula y
calcula (Mult x y) = calcula x * calcula y

infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico x) = "(" ++ "-" ++ infixa x ++ ")"
infixa (Mais x y) = "(" ++ infixa x ++ "+" ++ infixa y ++ ")"
infixa (Menos x y) = "(" ++ infixa x ++ "-" ++ infixa y ++ ")"
infixa (Mult x y) = "(" ++ infixa x ++ "*" ++ infixa y ++ ")"

posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico x) = posfixa x ++ " (-) "
posfixa (Mais x y) = posfixa x ++ " " ++ posfixa y ++ " +"
posfixa (Menos x y) = posfixa x ++ " " ++ posfixa y ++ " -" 
posfixa (Mult x y) = posfixa x ++ " " ++ posfixa y ++ " *"


---2
data RTree a = R a [RTree a]
    deriving (Show)

arvore :: Num a => RTree a
arvore = R 1 [R 2 [R 1 []],
              R 3 [],
              R 4 []]
   

--a
soma :: Num a => RTree a -> a
soma (R a resto) = a + sum (map soma resto)

--b
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x resto) = 1 + maximum (map altura resto)

--c 
prune :: Int -> RTree a -> RTree a
prune 0 (R a x) = R a []
prune y (R x arv) = R x (map (prune (y-1)) arv)

--d
mirror :: RTree a -> RTree a
mirror (R a []) = R a []
mirror (R a arv) = R a (map mirror (reverse arv))

--e
postorder :: RTree a -> [a]
postorder (R a []) = [a]
postorder (R a lista) = concatMap postorder lista  ++ [a] 


---3

data LTree a = Tip a | Fork (LTree a) (LTree a)

ltree :: Num a => LTree a
ltree = Fork (Fork (Tip 1) (Tip 2)) (Tip 3)

--a
ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e + ltSum d

--b
listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d

--c
ltHeight :: LTree a -> Int
ltHeight (Tip x) = 1
ltHeight (Fork e d) = max (1 + ltHeight e) (1 + ltHeight d)


--4
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
data BTree a = Empty | Node a (BTree a) (BTree a)

--a
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf n) = (Empty, Tip n)
splitFTree (No a l r) = (Node a lb rb, Fork ll rl)
    where (lb, ll) = splitFTree l
          (rb, rl) = splitFTree r 

--b
--(DUVIDA)--


