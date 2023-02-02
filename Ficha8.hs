module Ficha8 where

{-

-> Eq

>:i Eq  
    (==)


-> Ord
>:i Ord
    (<=) ou (compare)


-> Show
>:i Show
    (converter para String - show)


-> Num 
:i Num
    (+) (-) (*) negate abs signum fromInteger


instance _____________ where
    ...


-}



---1
data Frac = F Integer Integer

--a
normaliza :: Frac -> Frac
normaliza (F 0 y) = F 0 0
normaliza (F x y)
    | s > 0 = F a b
    | otherwise = F (-1*a) b 
    where d = mdc (abs x) (abs y)
          a = div (abs x) d
          b = div (abs y) d
          s = (signum x)*(signum y)

mdc :: Integer -> Integer -> Integer
mdc x y 
    | x == y = x
    | x > y = mdc (x-y) y
    | x < y = mdc x (y-x)

--b
instance Eq Frac where
    f1 == f2 = (a == c) && (b == d)
     where (F a b) = normaliza f1
           (F c d) = normaliza f2

--c
instance Ord Frac where
    f1 <= f2 = a*d <= c*b
     where (F a b) = normaliza f1
           (F c d) = normaliza f2

--d
instance Show Frac where
    --show :: Frac -> String
    show (F a b) = "(" ++ (show a) ++ "/" ++ (show b) ++ ")"

--e
instance Num Frac where
    (F a b) + (F x y) = F (a*y + b*x) (b*y)
    (F a b) * (F x y) = F (a*x) (b*y)
    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) = F (signum a *signum b) 1
    fromInteger n = F n 1

maioresDobro :: Frac -> [Frac] -> [Frac]
maioresDobro f = filter (>2*f) 


---2
data Exp a = Const a
    | Simetrico (Exp a)
    | Mais (Exp a) (Exp a)
    | Menos (Exp a) (Exp a)
    | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "(-" ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

valor :: Num a => Exp a -> a
valor (Const x) = x
valor (Simetrico x) = negate $ valor x
valor (Mais x y) = valor x + valor y
valor (Menos x y) = valor x - valor y
valor (Mult x y) = valor x * valor y

instance (Num a, Eq a) => Eq (Exp a) where
    (==) :: (Num a, Eq a) => Exp a -> Exp a -> Bool
    f1 == f2 = valor f1 == valor f2

--3

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]


instance Show Data where
    show (D x y z) = show x ++ "/" ++ show y ++ "/" ++ show z