--2





--3
data Contacto = Casa Integer
    | Trab Integer
    | Tlm Integer
    | Email String
    deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

--a
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail x y [] = [(x, [Email y])]
acrescEmail x y ((n,cs):t)
    | x == n = (n, Email y : cs) : t
    | otherwise = (n,cs) : acrescEmail x y t

--b
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome ((n,cs):t)
    | nome == n = Just (vAux cs)
    | otherwise =  verEmails nome t

vAux :: [Contacto] -> [String]
vAux [] = []
vAux (Email y:t) = y: vAux t
vAux (_:t) = vAux t


--c
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (Casa x: xs) = x:consTelefs xs
consTelefs (Trab x:xs) = x: consTelefs xs
consTelefs (Tlm x:xs) = x: consTelefs xs
consTelefs  (_:xs) = consTelefs xs

--d
casa :: Nome -> Agenda -> Maybe Integer
casa nome [] = Nothing
casa nome ((n,cs):t)
    | nome == n = vCasa cs
    | otherwise = casa nome t


vCasa :: [Contacto] -> Maybe Integer
vCasa [] = Nothing
vCasa (Casa x: xs) = Just x 
vCasa (_:t) = vCasa t


--4

type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String

--data Data = D Dia Mes Ano
    --deriving Show
type TabDN = [(Nome,Data)]

--a 
procura :: Nome -> TabDN -> Maybe Data
procura nome [] = Nothing
procura nome ((n,d):t)
    | nome == n = Just d
    | otherwise =  procura nome t

--b
calcAnos :: Data -> Data -> Int
calcAnos (D x y z) (D a b c)
    | z <= c = 0
    | z > c && y > b = z-c 
    | z > c && y == b && z > c = z - c 
    | otherwise = z - c -1


idade :: Data -> Nome -> TabDN -> Maybe Int
idade h nome [] = Nothing
idade h nome ((n,D x y z):t)
    | nome == n = Just (calcAnos h (D x y z))
    | otherwise = idade h nome t

--c
anterior :: Data -> Data -> Bool

{- anterior (D x y z) (D a b c)
    | z < c = True
    | z == c && y < b = True
    | z == c && y == b && x < a = True
    | otherwise = False
-}

anterior (D x y z) (D a b c) = z < c || (z == c && (y < b || (y == b && x < a)))

--d
  
ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):ts) = insereDN (n,d) (ordena ts)

insereDN :: (Nome,Data) -> TabDN -> TabDN
insereDN (n,d) [] = [(n,d)]
insereDN (n,d) ((nh,dh):t) | anterior d dh = (n,d) : (nh,dh) : t
                           | otherwise = (nh,dh) : insereDN (n,d) t

{-
| d > d1 = head t : ordena t ++ [(n,d)]
| otherwise = (n,d) : ordena t
    where d1 = snd (head t)

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):t) = ordenaux (n,d) (ordena t)



ordenaux :: (Nome,Data) -> TabDN -> TabDN
ornenaux (x,y) [] = [(x,y)]
ordenaux (x,y) ((n,h):xs)
    | anterior y h = (x,y): (n,h):
    | otherwise = (n,h): ordenaux (x,y) xs

   -}

--e
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade d list = porIdadeaux d (ordena list)


porIdadeaux :: Data -> TabDN -> [(Nome,Int)]
porIdadeaux d [] = []
porIdadeaux d ((x,y):t) = porIdadeaux d t ++ [(x, calcAnos d y)]


--5
data Movimento = Credito Float | Debito Float
    deriving Show

data Data = D Int Int Int
    deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
    deriving Show

--a
{-

desloc :: Int -> [Int] -> [Int]
desloc 0 (h:t) = (h:t)
desloc _ [] = []
desloc x (y:ys) = desloc (x-1) ys ++ [x]

-}

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext f []) x = []
extValor (Ext f ((_,_,m):t)) x
    | x< valorMov (m) = m: extValor (Ext f t) x

valorMov :: Movimento -> Float
valorMov (Credito x) = x
valorMov (Debito x) = x