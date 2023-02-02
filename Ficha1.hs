
import Data.Char


--Exercicio 1

--a
perimetro :: Float -> Float
perimetro r = 2*pi*r

--b
dist :: (Float,Float) -> (Float,Float) -> Float
dist (x1,y1) (x2,y2) = sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))

--c
primUlt :: [Int] -> Hora
primUlt l = (head l,last l)

--d
multiplo :: Int -> Int -> Bool
multiplo m n 
    | mod m n == 0 = True
    | otherwise = False

--e
truncaImpar :: [Int] -> [Int]
truncaImpar l
    | mod (length l) 2 == 0 = l
    | otherwise = tail l

--f
max2 :: Int -> Int -> Int 
max2 x y 
    | x >= y = x
    | otherwise = y

--g
max3 :: Int -> Int -> Int -> Int 
max3 x y z 
    | max2 x y <= z = z
    | otherwise = max2 x y


--Exercicio 2

nraizes :: Float -> Float -> Float -> Float 
nraizes a b c
    | (b*b - 4*a*c) < 0 = 0
    | (b*b - 4*a*c) == 0 = 1
    | otherwise = 2

raizes :: Float -> Float -> Float -> [Float]
raizes a b c
    | nraizes a b c == 0 = []
    | nraizes a b c == 1 = [(-b)/(2*a)]
    | nraizes a b c == 2 = [(-b +(b*b - 4*a*c))/(2*a),(-b -(b*b - 4*a*c))/(2*a)]


--Exercicio 3

type Hora = (Int,Int)

hora:: Hora -> Bool
hora (ho,mi) = elem ho [0..23] && elem mi [0..59] 

horaMaior :: Hora -> Hora -> Bool
horaMaior (h1,m1) (h2,m2)
    | h1 > h2 = True
    | h1 == h2 && m1 > m2 = True
    | otherwise = False

convertHoras :: Hora -> Int
convertHoras (h,m) = h*60 + m

convertMinutos :: Int -> Hora
convertMinutos x = (div x 60 , mod x 60)

difHoras :: Hora -> Hora -> Int
difHoras (h1,m1) (h2,m2) = abs(convertHoras (h1,m1) - convertHoras(h2,m2))

sumMin :: Int -> Hora -> Hora
sumMin min (h,m)
    | min < 60 = (h,m + min)
    | otherwise = convertMinutos(convertHoras (h,m) + min)


--Exercicio 4
--(APENAS MUDAR VARIAVEIS EXERCICIO 3)

--Exercicio 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next s
    | s == Verde = Amarelo
    | s == Amarelo = Vermelho
    | otherwise = Verde

stop :: Semaforo -> Bool
stop s
    | s == Vermelho || s == Amarelo = True
    | otherwise = False

safe :: Semaforo -> Semaforo -> Bool
safe s1 s2
    | (s1 == Verde && s2 == Vermelho) || (s1 == Vermelho && s2 == Verde) = True
    | otherwise = False

--Exercicio 6

data Ponto = Cartesiano Double Double | Polar Double Double
            deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = cos a * r 

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a ) = r * sin a

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x*x+y*y)
raio (Polar r a) = r

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar r a) = a 

convertPolar :: Ponto -> Ponto
convertPolar (Polar r a) = Cartesiano (r * cos a) (r * sin a)

dist2 :: Ponto -> Ponto -> Double
dist2 (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))


--Exercicio 7

data Figura = Circulo Ponto Double
    | Rectangulo Ponto Ponto
    | Triangulo Ponto Ponto Ponto
    deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo _ _ ) = False
poligono (Rectangulo p1 p2) = posx p1 /= posx p2 && posy p1 /= posy p2
poligono (Triangulo p1 p2 p3) = posx p1 /= posx p2 ||
                                posx p2 /= posx p3 ||
                                posx p1 /= posx p3
                                &&
                                posy p1 /= posy p2 ||
                                posy p2 /= posy p3 ||
                                posy p1 /= posy p3


vertices :: Figura -> [Ponto]
vertices (Circulo p r) = []
vertices (Rectangulo p1 p2) = [p1 , p2 , Cartesiano (posx p1) (posy p2), Cartesiano (posx p2) (posy p1)]
vertices (Triangulo x y z) = [x,y,z]

perimetro1:: Figura -> Double
perimetro1 (Circulo _ r) = 2 * pi * r
perimetro1 (Rectangulo p1 p2) = abs (posx p2 - posx p1) * 2 + abs (posy p2 - posy p1) * 2
perimetro1 (Triangulo p1 p2 p3) = dist2 p1 p2 + dist2 p2 p3 + dist2 p1 p3

----------------------DUVIDAS NO 7--------------------------------

--Ex8

isLower :: Char -> Bool
isLower c = ord c >= ord 'a' && ord c <= ord 'z'


isDigit :: Char -> Bool
isDigit a
    | ord a >= ord '0' && ord a <= ord '9' = True
    | otherwise  = False

isAlpha :: Char -> Bool
isAlpha a
    | ord a >= ord 'A' && ord a <= ord 'z' = True
    | otherwise = False

toUpper :: Char -> Char
toUpper c = chr(ord c - 32)

intToDigit :: Int -> Char
intToDigit x = chr(x + 48)

digitToInt :: Char -> Int
digitToInt x = ord x -48

vazia :: [a] -> Bool
vazia [] = True
vazia _ = False 




