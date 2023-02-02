--imports
import Data.Char


--Ex1



---Ex2

--a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h: dobros t

--b)
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
--numOcorre c (x:xs) = if (c==x) then 1 + numOcorre c xs else numOcorre c xs
numOcorre c (x:xs)
    |c == x = 1 + numOcorre c xs
    |otherwise = numOcorre c xs

--c)
positivos :: [Float] -> Bool
positivos [] = True
positivos (x:xs)
    | x < 0 = False
    | otherwise = positivos xs

--d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) = if x >= 0 then x : soPos xs else soPos xs

--e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs)
    | x < 0 =  x + somaNeg xs
    | otherwise =  somaNeg xs

--f)
tresUlt :: [a] -> [a]
tresUlt l
    | length l < 4 = l
    | otherwise = tresUlt (tail l)

--g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x1,x2):xs) = x2 : segundos xs

--h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a ((x1,_):xs) = if a == x1 then True else nosPrimeiros a xs

--i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos ((x,y,z):xs) = (x+a,y+b,z+c) 
    where (a,b,c) = sumTriplos xs

---Ex3

--a)
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) 
    | ord x >= 48  && ord x <= 57 = chr(ord x): soDigitos xs
    | otherwise = soDigitos xs

--b)
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs)
    | ord x >= 97 && ord x <= 122 = 1 + minusculas xs
    | otherwise = minusculas xs

--c)
nums :: String -> [Int]
nums [] = []
nums (x:xs) 
    | ord x >= 48  && ord x <= 57 = digitToInt x: nums xs
    | otherwise = nums xs

-- digitToInt '1' -> 1

---Ex4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((x,y):t) = if n == y then 1 + conta n t else conta n t

--b)
grau :: Polinomio -> Int
grau [] = 0
grau ((x,y):t) = max y (grau t)

--c)
selgrau ::  Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((x,y):t) = if n == y then (x,y): selgrau n t else selgrau n t

--d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,y):t) = if y == 0 then deriv t else (x * fromIntegral y,y - 1): deriv t

--e)
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula n ((x,y):t) = x*(n)^(y) + calcula n t

--f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,y):t) = if x /= 0 then (x,y): simp t else simp t

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult m [] = []
mult (x1,x2) ((a,b):xs) = (x1 * a, x2 + b): mult (x1,x2) xs

--h)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,g):t) = normalizaAux (c,g) (normaliza t)

normalizaAux :: Monomio -> Polinomio -> Polinomio
normalizaAux m [] = [m]
normalizaAux (cm,gm) ((c,g):t)
    | gm == g = (cm + c,g) : t
    | otherwise = (c,g) : normalizaAux (cm,gm) t

--i)
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (m:t) p = (mult m p) ++ produto t p

--j)
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (m:t) = insere m (ordena t)

insere :: Monomio -> Polinomio -> Polinomio
insere m [] = [m]
insere (cm,gm) ((c,g):t)
    | g > gm = (cm,gm) : (c,g) : t
    | otherwise = (c,g) : insere (cm,gm) t

--k)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)
