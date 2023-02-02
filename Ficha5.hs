module Ficha5 where
import Data.List
-----1
--a
any1 :: (a -> Bool) -> [a] -> Bool
any1 x [] = False
any1 x (h:t) = x h || any x t

--b
zipWith1 :: (a->b->c) -> [a] -> [b] -> [c]
zipWith1 funcao (x:xs) (y:ys) = funcao x y: zipWith1 funcao xs ys
zipWith1 _ _ _ = []

--c
takeWhile1 :: (a->Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 f (h:t)
    | f h = h: takeWhile1 f t
    | otherwise = []

--d
dropWhile1 :: (a->Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 f (h:t)
    | f h = dropWhile1 f t
    | otherwise = h:t

--e
span1 :: (a-> Bool) -> [a] -> ([a],[a])
span1 _ [] = ([],[])
span1 f (h:t)
    | f h = (h:a,b)
    | otherwise =  ([],h:t)
    where (a,b) = span1 f t

--e
deleteBy1 :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy1 f x (h:t) 
    | f x h = t
    | otherwise = h : deleteBy1 f x t

--f
sortOn1 :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn1 _ [] = []
sortOn1 f t = foldr (insertOn1 f) [] t
    
insertOn1 :: (Ord b) => (a -> b) -> a -> [a] -> [a]
insertOn1 _ x [] = [x]
insertOn1 f x (h:t) = if f x > f h then h : insertOn1 f x t else x : h : t


---2
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a
selgrau :: Int -> Polinomio -> Polinomio
selgrau g = filter (\x -> snd x == g)

--b
conta :: Int -> Polinomio -> Int
conta g l = length (filter (\x -> snd x == g) l)

--c
grau :: Polinomio -> Int
grau p = maximum (map snd p)

--d
deriv :: Polinomio -> Polinomio
deriv p = map (\(c,g) -> (c * fromIntegral g, g - 1)) $ filter (\(_,g) -> g /= 0) p

--e
calcula :: Float -> Polinomio -> Float
calcula x p = sum (map (\(c,g) -> c * x ^ g) p)

--f
simp :: Polinomio -> Polinomio
simp = filter (\(c,_) -> c/= 0) 

--g
mult :: Monomio -> Polinomio -> Polinomio
mult (c1,g1) =  map (\(c,g) -> (c1*c,g1+g))

--h
ordena :: Polinomio -> Polinomio
ordena = sortOn1 snd 

--i
normaliza :: Polinomio -> Polinomio
normaliza = foldl (\acc m -> adiciona m acc) [] 
    where adiciona :: Monomio -> Polinomio -> Polinomio
          adiciona m [] = [m]
          adiciona (cm,gm) ((c,g):t) = if gm == g then (cm+c,g) : t else (c,g) : adiciona (cm,gm) t

--j
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza ((++) p1 p2)

--k
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldl (\acc x -> soma (mult x p2) acc) [] p1

--l
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = if simp (soma p1 (mult (-1,0) p2)) == [] then True else False

---3

type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK m = length (nub (map length m)) == 1

dimMat :: Mat a -> (Int, Int)
dimMat m = (length m, length (head m))

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (x:xs) (y:ys) = zipWith (+) x y : addMat xs ys

transpose1 :: Mat a -> Mat a
transpose1 m = [map (!! i) m |  i <- [0..c-1]]
    where (l,c) = dimMat m

multMat :: Mat a -> Mat a -> Mat a
multMat = undefined