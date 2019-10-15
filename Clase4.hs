--Ejercicios: otras sumatorias

f1 :: Integer -> Integer
f1 n | n==0 = 1
     | otherwise = 2^(n) + f1(n-1)

f2 :: Integer -> Float -> Float
f2 n q | n==1 = q
       | otherwise = q^n + f2 (n-1) q 

f3 :: Integer -> Float -> Float
f3 n q | n == 0 = 1
       | otherwise = q^(2*n) + f3 (n-1) q        

f4 :: Integer -> Float -> Float
f4 n q | n == 0 = 1
       | otherwise = q^(2*n - 1) + q^(2*n) - q^(n-1) + f4 (n-1) q 

--Otras funciones recursivas

--Funcion esPar

esPar :: Integer -> Bool
esPar n | n == 0 = True
        | n == 1 = False  -- el sentido de los casos bases esta relacionado con el resto de dividir un numero cualquiera por 2
        | otherwise = esPar (n-2) --se si el anterior menos 2 es par 

--Funcion esMultiploDe3

esmultiplode3 :: Integer -> Bool
esmultiplode3 n | n == 0 = True
                | n == 1 = False
                | n == 2 = False
                | otherwise = esmultiplode3(n - 3)

--Funcion sumaImpares

sumaImpares :: Integer -> Integer
sumaImpares n | n==1 = 1
              | esPar n = sumaImpares(n-1)
              | otherwise = n + sumaImpares (n-1)

--Funcion dobleFact

dobleFact :: Integer -> Integer
dobleFact n | n == 0 = 1
            | n == 1 = 1
            | otherwise = n * dobleFact (n - 2)

--Funcion infinitaNegativa

infinitaNegativa :: Integer -> Integer
infinitaNegativa n | n == 0 = 0
                   | otherwise = infinitaNegativa(n-1)