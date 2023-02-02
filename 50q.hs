
--------------------50Q-----------------------

--1
myenumFromTo2 :: Int -> Int -> [Int]
myenumFromTo2 x y
    | x > y = []
    | otherwise = x: myenumFromTo2 (x+ 1) y

--2
a :: Int -> Int -> Int -> [Int]
a x y z
    | x > z && y > z = []
    | x == y = [x]
    | otherwise = x: a y (2*y-x) z

--3
myconcatena :: [a] -> [a] -> [a]
myconcatena [] x = x
myconcatena (x:xs) t = x: myconcatena xs t

--4
myposicao :: [a] -> Int -> a
myposicao l 0 = head l
myposicao (h:t) x = myposicao t (x-1)

--5
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h:s) = myreverse s ++ [h]

--6
mytake :: Int -> [a] -> [a]
mytake 0 l = []
mytake x (h:t)
    | x > length (h:t) = []
    | otherwise = h: mytake (x-1) t
mytake _ _ = []

--7
mydrop :: Int -> [a] -> [a]
mydrop 0 l = l
mydrop x (h:t)
    | length (h:t) < x = []
    | otherwise = mydrop (x-1) t

--8
myzip :: [a] -> [b] -> [(a,b)]
myzip (h:t) (x:xs) = (h,x): myzip t xs
myzip _ _ = []

--9
myreplicate :: Int -> a -> [a]
myreplicate 0 y = []
myreplicate x y = y : myreplicate (x-1) y

--10
myintersperse :: a -> [a] -> [a]
myintersperse x [y] = [y]
myintersperse x (h:t) = h:x: myintersperse x t

--11
mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup [x] = [[x]]
mygroup l = u: mygroup (drop (length u) l)
    where u = groupaux l

groupaux :: Eq a => [a] -> [a]
groupaux [] = []
groupaux [x] = [x]
groupaux (h:h1:t)
    | h == h1 = h: groupaux (h1:t) 
    | otherwise = [h] 

--12
myconcat :: [[a]] -> [a]
myconcat ([]:t) = myconcat t
myconcat [] = []
myconcat (h:t) = h ++ myconcat t

--13
inits :: [a] -> [[a]]
inits [] = [[]]
inits (h:t) = inits t ++ [(h:t)]


--14
mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails (h:t) = (h:t): mytails t

--15
myheads :: [[a]] -> [a]
myheads [] = []
myheads ([]:t) = myheads t
myheads (h:t) = head h : myheads t

--16
mytotal :: [[a]] -> Int
mytotal [] = 0
mytotal (h:t) = (length h) + mytotal t

--17
myfun :: [(a,b,c)] -> [(a,c)]
myfun [] = []
myfun (x:t) = (selectioned x): myfun t

selectioned :: (a,b,c) -> (a,c)
selectioned (a,b,c) = (a,c)

--18
mycola :: [(String,b,c)] -> String
mycola [] = ""
mycola ((a,b,c):t) = a ++ mycola t

--19
myidade :: Int -> Int -> [(String,Int)] -> [String]
myidade x y [] = []
myidade x y ((a,b):t)
    | x-y >= b = a: myidade x y t
    |otherwise = myidade x y t

--20
mypowerEnumFrom :: Int -> Int -> [Int]
mypowerEnumFrom x 1 = [1]
mypowerEnumFrom x y = mypowerEnumFrom x (y-1) ++ [x^(y-1)]
{-
mypowerEnumFrom x 1 = [1]
mypowerEnumFrom n m = paux n m 0

paux :: Int -> Int -> Int ->  [Int]
paux n m ac
    | m > ac = n^(ac): paux n m (ac+1)
    | otherwise = []
-}
--21
myisPrime :: Int -> Bool
myisPrime x
    |x >= 2 = primeaux x 2
    |otherwise = False

primeaux :: Int -> Int -> Bool
primeaux n m 
    | mod n m == 0 = False
    | m * m > n = True
    | otherwise = primeaux n (m+1)

--22
myisPrefix :: Eq a => [a] -> [a] -> Bool
myisPrefix x l
    | length l <= length x = False
    | init l == x = True
    | otherwise = False

--23
{-myisSuffix :: Eq a => [a] -> [a] -> Bool
myisSuffix x l
    | length l <= length x = False
    | drop 1 l == x = True
    | otherwise = False
-}
myisSuffix :: Eq a => [a] -> [a] -> Bool
myisSuffix [] [x] = True
myisSuffix (x:xs) (h:t) = x == head t && myisSuffix xs t
myisSuffix _ _ = False
--24
myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myisSubsequenceOf (h:t) (x:xs) = h == x && myisSubsequenceOf t xs || myisSubsequenceOf (h:t) xs

--25
myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices x l = elemAux x l 0

elemAux :: Eq a => a -> [a] -> Int -> [Int]
elemAux _ [] _ = []
elemAux x (h:t) i 
    | x == h = i : elemAux x t (i+1)
    | otherwise = elemAux x t (i+1)

--26
mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub lista = aux lista []

aux :: Eq a => [a] -> [a] -> [a]
aux [] [] = []
aux (h:t) ac
    | elem h ac == False = aux t (h:ac)
    | otherwise = aux t ac


--27
mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete x (y:ys)
    | x == y = ys
    | otherwise = y: mydelete x ys

--28
myremove :: Eq a => [a] -> [a] -> [a]
myremove l [] = l
myremove [] _ = []
myremove l (h:t) = myremove (mydelete h l) t

--29

myunion :: Eq a => [a] -> [a] -> [a]
myunion l [] = l
myunion l (h:t)
    | elem h l = myunion l t
    | otherwise = h: myunion l t

--30
myintersect :: Eq a => [a] -> [a] -> [a]
myintersect (h:t) l = if h `elem` l then h: myintersect t l else myintersect t l 
myintersect  _ _ = []

--31
myinsert :: Ord a => a -> [a] -> [a]
myinsert a []= [a]
myinsert x (h:t) = if x < h then x:h:t else h: myinsert x t

--32
myunwords :: [String] -> String
myunwords [x] = x
myunwords (h:t) = h ++ " " ++ myunwords t

--33
myunlines :: [String] -> String
myunlines [] = ""
myunlines (h:t) = h ++ "/n" ++ myunlines t

--34
pMaior :: Ord a => [a] -> Int 
--pMaior [] = error "lista vazia"
--pMaior [x] = 0
--pMaior (h:t) | h >= (t !! pMaior t) = 0
--             | otherwise = 1 + pMaior t
pMaior lista = paux lista 0

paux :: Ord a => [a] -> Int -> Int
paux [] x = x
paux (h:t) x
    | x < h = paux t x
    | otherwise = paux t (x+1)



{-}
paux :: Ord a => [a] -> Int -> Int
paux [x] _ = 0
paux (h:h1:t) ac
    | h < h1 = (ac +1) + paux (h1:t) ac 
    | otherwise = paux (h1:t) (ac) + paux t 
-}

--35
mylookup :: Eq a => a -> [(a,b)] -> Maybe b
mylookup _ [] = Nothing
mylookup n ((x,y):t) = if n == x then Just y else mylookup n t

--36
mypreCrescente :: Ord a => [a] -> [a]
mypreCrescente [] = []
mypreCrescente [x] = [x]
mypreCrescente (h:h1:t) = if h <= h1 then h: mypreCrescente (h1:t) else [h]

--37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = myinsert h (iSort t)

--38
mymenor :: String -> String -> Bool
mymenor (h:t) [] = False
mymenor [] (x:xs) = True
mymenor h x = h <= x


--39

myelemMSet :: Eq a => a -> [(a,Int)] -> Bool
myelemMSet x [] = False
myelemMSet x ((y,_):t)
    | x == y = True
    | otherwise = myelemMSet x t

--40
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,i):t)
    | i == 0 = converteMSet t 
    | otherwise = x:converteMSet ((x,i-1):t)

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((n,i):t) 
    | x == n = (n,i+1):t
    | otherwise = (n,i):insereMSet x t

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((n,i):t)
    | x == n && i > 1 = (n,i-1):removeMSet x t
    | x == n && i == 1 = removeMSet x t
    | otherwise = (n,i):removeMSet x t

--43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t)

--44
partitionEithers1 :: [Either a b] -> ([a],[b])
partitionEithers1 [] = ([],[])
partitionEithers1 (Left x:xs) = (x:a,b)
    where (a,b) = partitionEithers1 xs
partitionEithers1 (Right x:xs) = (a,x:b)
    where (a,b) = partitionEithers1 xs


--45
catMaybes1 :: [Maybe a] -> [a]
catMaybes1 [] = []
catMaybes1 (h:t) = case h of Nothing -> catMaybes1 t
                             Just x -> x: catMaybes1 t


--46
data Movimento = Norte | Sul | Este | Oeste
    deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a,b) (x,y) 
    |a == x && b == y = []
caminho (a, b) (x, y) 
    | a < x = Este : caminho ( a + 1, b) (x, y)
    | a > x = Oeste : caminho (a - 1, b) (x, y)
    | b < y = Norte : caminho (a, b + 1) (x, y)
    | b > y = Sul : caminho (a, b - 1) (x, y)
    




        {- (a,b) (x,y)
    | a < x && b < y = Norte:Este:caminho (a+1,b+1) (x,y)
    | a < x && b > y = Norte:Oeste:caminho (a+1,b-1) (x,y)
    | a > x && b < y = Sul:Este:caminho (a-1,b+1) (x,y)
    | a > x && b > y = Norte:Oeste:caminho (a-1,b-11) (x,y)
    | a == x && b < y = Este:caminho (a,b+1) (x,y)
    | a == x && b > y = Oeste:caminho (a,b-1) (x,y)
    | a < x && b == y = Norte:caminho (a+1,b) (x,y)
    | a > x && b == y = Sul:caminho (a-1,b) (x,y)  -}

--47
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x, y) (Norte:t) = posicao (x, y + 1) t
posicao (x, y) (Sul:t) = posicao (x, y - 1) t
posicao (x, y) (Este:t) = posicao (x + 1, y) t
posicao (x, y) (Oeste:t) = posicao (x - 1, y) t

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops posi movs = posi == posicao posi movs || hasLoops posi (init movs)


--48
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (h:t) 
    | eQuadrado h = 1 + contaQuadrados t
    | otherwise = contaQuadrados t

eQuadrado :: Rectangulo -> Bool
eQuadrado (Rect (x1,y1) (x2,y2)) = abs (y2 - y1) == abs (x2 - x1)

--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs (x2 - x1) * abs (y2 - y1) + areaTotal t



--50
data Equipamento = Bom | Razoavel | Avariado
    deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t


