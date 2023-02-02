
--Imports
import Data.Char 


--Exercicios

-- 1
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t)
    | isDigit h = (h:a,b) 
    | isAlpha h = (a,h:b)
    where (a,b) = digitAlpha t

digitAlpha1 :: String -> (String,String)
digitAlpha1 [] = ([],[])
digitAlpha1 s = (filter isAlpha s, filter isDigit s)

-- 2
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp lista = nzpaux (0,0,0) lista 

nzpaux :: (Int,Int,Int) -> [Int] -> (Int,Int,Int)
nzpaux (a,b,c) [] = (a,b,c)
nzpaux (a,b,c) (h:t)
    | h < 0 = nzpaux (a+1,b,c) t
    | h == 0 = nzpaux (a,b+1,c) t
    | h > 0 = nzpaux (a,b,c+1) t

nzpOS  :: [Int] -> (Int,Int,Int)
nzpOS l@(h:t) = (length (filter (<0) l), length(filter (==0) l),length (filter (>0) l))

nzpFl :: [Int] -> (Int,Int,Int)
nzpFl l = foldl (\(n,z,p) x -> if x < 0 then (n+1,z,p) else if x == 0 then (n,z+1,p) else (n,z,p+1)) (0,0,0) l 

 
-- 3

divMod1 :: Integral a => a -> a -> (a, a)
divMod1 x y
    | x - y < 0 = (0,x)
    | otherwise = (d+1,r)
    where (d,r) = divMod1 (x - y) y
{-}
divMod1 :: Integral a => a -> a -> (a, a)
divMod1 x y = (div x y,mod x y)
-}

-- 4
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = auxfd t h

auxfd :: [Int] -> Int -> Int
auxfd [] ac = ac
--auxfd (h:t) ac = auxfd t (ac*10 + h)
auxfd t ac = foldl (\ ac h -> ac * 10 + h) ac t

-- 5
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits1 l]

inits1 :: [a] -> [[a]]
inits1 [] = []
inits1 [x] = [[x]]
inits1 l = inits1 (init l) ++ [l]

maxSumInit1 :: (Num a, Ord a) => [a] -> a
maxSumInit1 l = maximum (auxM l) 

auxM :: (Num a, Ord a) => [a] -> [a]
auxM [] = []
auxM l = sum (head (inits1 l)): auxM (head(tail (inits1 l)))


-- 6


--8

{-
.a    [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]  --> [6,12,18]
.b    [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]  --> [2,4,6,8,10,12,14,16,18,20] && mod x 3 == 0  --> [6,12,18]
.c    [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]  --> [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
.d    [sum [y | y <- [1..x], odd y] | x <- [1..10]] --> sum (1,3,5,7,9) --> 25
-}
--8
mod2mod3 :: [Int] -> [Int]
mod2mod3 [] = []
mod2mod3 (h:t)
    | mod h 2 == 0 && mod h 3 == 0 = h:  mod2mod3 t
    | otherwise = mod2mod3 t

soma30 :: [Int] -> [Int] -> [(Int,Int)]
soma30 (h:t) (x:xs)
    | last (h:t) + x == 30 = (last (h:t),x): soma30 t xs
    | otherwise = soma30 t xs
soma30 _ _ = []

--9
{-
.a  [2^x| x <- [1..10]]
.b  [(a,b) | a <- [1,2,3,4,5], b <- [5,4,3,2,1], a + b == 6]
.c  [[1..x]| x <- [1..5]]
.d  [replicate x 1 | x <- [1,5]]
e.  [product [y| y <- [1..x]] | x <- [1..6]]

-}
